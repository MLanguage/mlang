(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

open Bir

type offset =
  | GetValueConst of int
  | GetValueVar of variable
  | PassPointer
  | None

let rec generate_variable (vm : Dgfip_varid.var_id_map) (offset : offset)
    ?(def_flag = false) ?(debug_flag = false) (var : Bir.variable) : string =
  let mvar = Bir.var_to_mir var in
  try
    match offset with
    | PassPointer ->
        if def_flag then Dgfip_varid.gen_access_def_pointer vm mvar
        else Dgfip_varid.gen_access_pointer vm mvar
    | _ ->
        let offset =
          match offset with
          | None -> ""
          | GetValueVar offset -> " + (int)" ^ generate_variable vm None offset
          | GetValueConst offset -> " + " ^ string_of_int offset
          | PassPointer -> assert false
        in
        if def_flag then Dgfip_varid.gen_access_def vm mvar offset
        else
          let access_val = Dgfip_varid.gen_access_val vm mvar offset in
          if debug_flag then
            let vn = Pos.unmark mvar.Mir.Variable.name in
            let pos_tgv = Dgfip_varid.gen_access_pos_from_start vm mvar in
            Format.asprintf "(aff3(\"%s\",irdata, %s), %s)" vn pos_tgv
              access_val
          else access_val
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark mvar.Mir.Variable.name))

type dflag = Def | Val

type local_var = string * int

type expr_var = Local of local_var | M of variable * offset

type dexpr =
  | Done
  | Dzero
  | Dlit of float
  | Dvar of expr_var * dflag
  | Dand of dexpr * dexpr
  | Dor of dexpr * dexpr
  | Dunop of string * dexpr
  | Dbinop of string * dexpr * dexpr
  | Dfun of string * dexpr list
  | Daccess of variable * local_var
  | Dite of dexpr * dexpr * dexpr

type expression_composition = {
  def_test : dexpr;
  value_comp : dexpr;
  subs : (local_var * expression_composition) list;
}

let format_local_var fmt (((vname, vnum), df) : local_var * dflag) =
  let def_suffix = match df with Def -> "_d" | Val -> "" in
  Format.fprintf fmt "%s_%d%s" vname vnum def_suffix

let format_expr_var (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) fmt ((ev, df) : expr_var * dflag) =
  match ev with
  | Local lvar -> format_local_var fmt (lvar, df)
  | M (var, offset) ->
      let def_flag = df = Def in
      Format.fprintf fmt "%s"
        (generate_variable ~debug_flag:dgfip_flags.flg_trace vm offset ~def_flag
           var)

let rec format_dexpr (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) fmt (de : dexpr) =
  let format_dexpr = format_dexpr dgfip_flags vm in
  match de with
  | Done -> Format.fprintf fmt "1"
  | Dzero -> Format.fprintf fmt "0"
  | Dlit f -> Format.fprintf fmt "%f" f
  | Dvar (evar, dflag) -> format_expr_var dgfip_flags vm fmt (evar, dflag)
  | Dand (de1, de2) -> format_dexpr fmt (Dbinop ("&&", de1, de2))
  | Dor (de1, de2) -> format_dexpr fmt (Dbinop ("||", de1, de2))
  | Dunop (op, de) ->
      Format.fprintf fmt "@[<hov 2>(@,%s%a@]@,)" op format_dexpr de
  | Dbinop (op, de1, de2) ->
      Format.fprintf fmt "@[<hov 2>(@,%a@ %s %a@]@,)" format_dexpr de1 op
        format_dexpr de2
  | Dfun (funname, des) ->
      Format.fprintf fmt "@[<hov 2>%s(%a@]@,)" funname
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           format_dexpr)
        des
  | Daccess (var, lvar) ->
      Format.fprintf fmt "(%s[(int)%a])"
        (generate_variable ~debug_flag:dgfip_flags.flg_trace vm PassPointer var)
        format_local_var (lvar, Val)
  | Dite (dec, det, dee) ->
      Format.fprintf fmt "@[<hov 2>(@,%a ?@ %a@ : %a@]@,)" format_dexpr dec
        format_dexpr det format_dexpr dee

let rec format_local_vars_defs (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) (fmt : Format.formatter)
    (defs : (local_var * expression_composition) list) : unit =
  List.iter
    (fun (lvar, se) ->
      Format.fprintf fmt "%a@;int %a = %a;@;double %a = %a;"
        (format_local_vars_defs dgfip_flags vm)
        se.subs format_local_var (lvar, Def)
        (format_dexpr dgfip_flags vm)
        se.def_test format_local_var (lvar, Val)
        (format_dexpr dgfip_flags vm)
        se.value_comp)
    defs

let fresh_c_local : string -> local_var =
  let c = ref 0 in
  fun name ->
    let v = (name, !c) in
    incr c;
    v

let rec simpl_dexpr (de : dexpr) : dexpr =
  match (de : dexpr) with
  | Dlit f -> begin match f with 0. -> Dzero | 1. -> Done | _ -> de end
  | Dand (de1, de2) -> begin
      match (simpl_dexpr de1, simpl_dexpr de2) with
      | Dzero, _ | _, Dzero -> Dzero
      | Done, de | de, Done -> de
      | de1, de2 -> Dand (de1, de2)
    end
  | Dor (de1, de2) -> begin
      match (simpl_dexpr de1, simpl_dexpr de2) with
      | Dzero, de | de, Dzero -> de
      | Done, _ | _, Done -> Done
      | de1, de2 -> Dor (de1, de2)
    end
  | Dunop ("!", de) -> begin
      match simpl_dexpr de with
      | Done -> Dzero
      | Dlit f when f <> 0. -> Dzero
      | Dzero -> Done
      | _ -> de
    end
  | Dite (dec, det, dee) -> begin
      match (simpl_dexpr dec, simpl_dexpr det, simpl_dexpr dee) with
      | Dzero, _, de | Done, de, _ -> de
      | dec, Done, Dzero -> dec
      | _ -> de
    end
  | Dunop (op, de) -> Dunop (op, simpl_dexpr de)
  | Dbinop (op, de1, de2) -> Dbinop (op, simpl_dexpr de1, simpl_dexpr de2)
  | Dfun (fname, des) -> Dfun (fname, List.map simpl_dexpr des)
  | Daccess _ | Done | Dzero | Dvar _ -> de

let simpl_expr_comp (e : expression_composition) : expression_composition =
  {
    e with
    def_test = simpl_dexpr e.def_test;
    value_comp = simpl_dexpr e.value_comp;
  }

let build_transitive_composition (e : expression_composition) :
    expression_composition =
  let expr_v = fresh_c_local "expr" in
  let e = simpl_expr_comp e in
  let def_test, use_subexpr =
    match e.def_test with
    | Dzero -> (Dzero, false)
    | Done -> (Done, false)
    | Dvar _ as test -> (test, false)
    | _ -> (Dvar (Local expr_v, Def), true)
  in
  let value_comp, use_subexpr =
    match e.value_comp with
    | Dzero | Done | Dlit _ | Dvar _ -> (e.value_comp, use_subexpr)
    | _ -> (Dvar (Local expr_v, Val), true)
  in
  let value_comp, use_subexpr =
    if def_test = Done then (value_comp, use_subexpr)
    else (Dite (Dvar (Local expr_v, Def), value_comp, Dzero), true)
  in
  let subs = if use_subexpr then [ (expr_v, e) ] else e.subs in
  { def_test; value_comp; subs }

let rec generate_c_expr (e : expression Pos.marked)
    (var_indexes : Dgfip_varid.var_id_map) : expression_composition =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = Dand (se1.def_test, se2.def_test) in
      let value_comp =
        let op =
          match Pos.unmark op with
          | Mast.Gt -> ">"
          | Mast.Gte -> ">="
          | Mast.Lt -> "<"
          | Mast.Lte -> "<="
          | Mast.Eq -> "=="
          | Mast.Neq -> "!="
        in
        Dbinop (op, se1.value_comp, se2.value_comp)
      in
      build_transitive_composition
        { def_test; value_comp; subs = se1.subs @ se2.subs }
  | Binop ((Mast.Div, _), e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = Dand (se1.def_test, se2.def_test) in
      let value_comp =
        Dite
          ( Dunop ("!", se2.value_comp),
            Dzero,
            Dbinop ("/", se1.value_comp, se2.value_comp) )
      in
      build_transitive_composition
        { def_test; value_comp; subs = se1.subs @ se2.subs }
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test =
        match Pos.unmark op with
        | Mast.And | Mast.Mul -> Dand (se1.def_test, se2.def_test)
        | Mast.Or | Mast.Add | Mast.Sub -> Dor (se1.def_test, se2.def_test)
        | Mast.Div -> assert false
        (* see above *)
      in
      let op e1 e2 =
        match Pos.unmark op with
        | Mast.And -> Dand (e1, e2)
        | Mast.Or -> Dor (e1, e2)
        | Mast.Add -> Dbinop ("+", e1, e2)
        | Mast.Sub -> Dbinop ("-", e1, e2)
        | Mast.Mul -> Dbinop ("*", e1, e2)
        | Mast.Div -> assert false
        (* see above *)
      in
      let value_comp = op se1.value_comp se2.value_comp in
      build_transitive_composition
        { def_test; value_comp; subs = se1.subs @ se2.subs }
  | Unop (op, e) ->
      let se = generate_c_expr e var_indexes in
      let def_test = se.def_test in
      let op = match op with Mast.Not -> "!" | Mast.Minus -> "-" in
      let value_comp = Dunop (op, se.value_comp) in
      build_transitive_composition { def_test; value_comp; subs = se.subs }
  | Index (var, e) ->
      let idx = generate_c_expr e var_indexes in
      let size =
        Option.get (Bir.var_to_mir (Pos.unmark var)).Mir.Variable.is_table
      in
      let idx_var = fresh_c_local "idx" in
      let def_test =
        Dand
          ( Dvar (Local idx_var, Def),
            Dbinop ("<=", Dvar (Local idx_var, Val), Dlit (float_of_int size))
          )
      in
      let value_comp =
        Dite
          ( Dbinop ("<", Dvar (Local idx_var, Val), Dzero),
            Dzero,
            Daccess (Pos.unmark var, idx_var) )
      in
      build_transitive_composition
        { def_test; value_comp; subs = [ (idx_var, idx) ] }
  | Conditional (c, t, f) ->
      let cond = generate_c_expr c var_indexes in
      let thenval = generate_c_expr t var_indexes in
      let elseval = generate_c_expr f var_indexes in
      let cond_var = fresh_c_local "cond" in
      let def_test =
        Dor
          ( Dand (Dvar (Local cond_var, Val), thenval.def_test),
            Dand (Dvar (Local cond_var, Def), elseval.def_test) )
      in
      let value_comp =
        Dite
          ( Dvar (Local cond_var, Def),
            Dite
              ( Dvar (Local cond_var, Val),
                thenval.value_comp,
                elseval.value_comp ),
            Dzero )
      in
      simpl_expr_comp
        {
          def_test;
          value_comp;
          subs = ((cond_var, cond) :: thenval.subs) @ elseval.subs;
        }
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = Done in
      let value_comp = se.def_test in
      build_transitive_composition { def_test; value_comp; subs = se.subs }
  | FunctionCall (NullFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Dand (def_test, Dbinop ("==", se.value_comp, Dzero)) in
      build_transitive_composition { def_test; value_comp; subs = se.subs }
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Dfun ("my_arr", [ se.value_comp ]) in
      build_transitive_composition { def_test; value_comp; subs = se.subs }
  | FunctionCall (InfFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Dfun ("my_floor", [ se.value_comp ]) in
      build_transitive_composition { def_test; value_comp; subs = se.subs }
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = Done in
      let value_comp = Dfun ("_fmax", [ se1.value_comp; se2.value_comp ]) in
      build_transitive_composition
        { def_test; value_comp; subs = se1.subs @ se2.subs }
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = Done in
      let value_comp = Dfun ("_fmin", [ se1.value_comp; se2.value_comp ]) in
      build_transitive_composition
        { def_test; value_comp; subs = se1.subs @ se2.subs }
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr e1 var_indexes in
      let bound_var = fresh_c_local "bound" in
      let def_test =
        Dfun
          ( "multimax_def",
            [ Dvar (Local bound_var, Val); Dvar (M (v2, PassPointer), Def) ] )
      in
      let value_comp =
        Dfun
          ( "multimax",
            [ Dvar (Local bound_var, Val); Dvar (M (v2, PassPointer), Val) ] )
      in
      simpl_expr_comp { def_test; value_comp; subs = [ (bound_var, bound) ] }
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) ->
      simpl_expr_comp { def_test = Done; value_comp = Dlit f; subs = [] }
  | Literal Undefined -> { def_test = Dzero; value_comp = Dzero; subs = [] }
  | Var var ->
      {
        def_test = Dvar (M (var, None), Def);
        value_comp = Dvar (M (var, None), Val);
        subs = [];
      }
  | LocalVar lvar ->
      let lvar = ("mlocal", lvar.Mir.LocalVariable.id) in
      {
        def_test = Dvar (Local lvar, Def);
        value_comp = Dvar (Local lvar, Val);
        subs = [];
      }
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let local_var = ("mlocal", lvar.Mir.LocalVariable.id) in
      let def_test = se2.def_test in
      let value_comp = se2.value_comp in
      { def_test; value_comp; subs = (local_var, se1) :: se2.subs }

let generate_var_def (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable)
    (data : variable_data) (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      let se = generate_c_expr e var_indexes in
      Format.fprintf oc "%a%s = %a;@\n%s = %a;@\n%s"
        (format_local_vars_defs dgfip_flags var_indexes)
        se.subs
        (generate_variable ~def_flag:true var_indexes None var)
        (format_dexpr dgfip_flags var_indexes)
        se.def_test
        (generate_variable var_indexes None var)
        (format_dexpr dgfip_flags var_indexes)
        se.value_comp
        (if dgfip_flags.flg_trace then
         let var = Bir.var_to_mir var in
         Format.asprintf "aff2(\"%s\", irdata, %s);@\n"
           (Pos.unmark var.Mir.Variable.name)
           (Dgfip_varid.gen_access_pos_from_start var_indexes var)
        else "")
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          Mir.IndexMap.iter (fun i v ->
              let sv = generate_c_expr v var_indexes in
              Format.fprintf fmt "@[<hov 2>{@;%a%s = %a;@\n%s = %a;@\n@]@,}@;"
                (format_local_vars_defs dgfip_flags var_indexes)
                sv.subs
                (generate_variable ~def_flag:true var_indexes (GetValueConst i)
                   var)
                (format_dexpr dgfip_flags var_indexes)
                sv.def_test
                (generate_variable var_indexes (GetValueConst i) var)
                (format_dexpr dgfip_flags var_indexes)
                sv.value_comp))
        es
  | TableVar (_size, IndexGeneric (v, e)) ->
      (* TODO: boundary checks *)
      let sv = generate_c_expr e var_indexes in
      Format.fprintf oc "if(%s)@[<hov 2>{%a%s = %a;@ %s = %a;@]@;}@\n"
        (generate_variable var_indexes None ~def_flag:true v)
        (format_local_vars_defs dgfip_flags var_indexes)
        sv.subs
        (generate_variable ~def_flag:true var_indexes (GetValueVar v) var)
        (format_dexpr dgfip_flags var_indexes)
        sv.def_test
        (generate_variable var_indexes (GetValueVar v) var)
        (format_dexpr dgfip_flags var_indexes)
        sv.value_comp
  | InputVar -> assert false

let generate_var_cond (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (cond : condition_data)
    (oc : Format.formatter) =
  let scond = generate_c_expr cond.cond_expr var_indexes in
  let erreur = Pos.unmark (fst cond.cond_error).Mir.Error.name in
  let code =
    match snd cond.cond_error with
    | None -> "NULL"
    | Some v ->
        Format.sprintf "\"%s\""
          (Pos.unmark (Bir.var_to_mir v).Mir.Variable.name)
  in
  Format.fprintf oc
    {|
    %acond_def = %a;
    cond = %a;
    if (cond_def && (cond != 0.0))
    {
      add_erreur(irdata, &erreur_%s, %s);
    }
|}
    (format_local_vars_defs dgfip_flags var_indexes)
    scond.subs
    (format_dexpr dgfip_flags var_indexes)
    scond.def_test
    (format_dexpr dgfip_flags var_indexes)
    scond.value_comp erreur code

let rec generate_stmt (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter) (stmt : stmt)
    =
  Format.fprintf oc "@[<hov 2>{@;";
  (match Pos.unmark stmt with
  | SAssign (var, vdata) ->
      generate_var_def dgfip_flags var_indexes var vdata oc
  | SConditional (cond, iftrue, iffalse) ->
      let cond_v = fresh_c_local "mpp_cond" in
      let cond = generate_c_expr (Pos.same_pos_as cond stmt) var_indexes in
      Format.fprintf oc "%a@[<hov 2>%a = %a;@]@;@[<hov 2>%a = %a;@]@;"
        (format_local_vars_defs dgfip_flags var_indexes)
        cond.subs format_local_var (cond_v, Def)
        (format_dexpr dgfip_flags var_indexes)
        cond.def_test format_local_var (cond_v, Val)
        (format_dexpr dgfip_flags var_indexes)
        cond.value_comp;
      Format.fprintf oc "@[<hv 2>if(%a && %a){@,%a@]@,}@;" format_local_var
        (cond_v, Def) format_local_var (cond_v, Val)
        (generate_stmts dgfip_flags program var_indexes)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<hv 2>else if(%a){@,%a@]@,}@;" format_local_var
          (cond_v, Def)
          (generate_stmts dgfip_flags program var_indexes)
          iffalse
  | SVerif v -> generate_var_cond dgfip_flags var_indexes v oc
  | SRovCall r ->
      let rov = ROVMap.find r program.rules_and_verifs in
      generate_rov_function_header ~definition:false oc rov
  | SFunctionCall (f, _) -> Format.fprintf oc "%s(irdata);\n" f);
  Format.fprintf oc "@]@,}@;"

and generate_stmts (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmts : stmt list) =
  Format.pp_print_list (generate_stmt dgfip_flags program var_indexes) oc stmts

and generate_rov_function_header ~(definition : bool) (oc : Format.formatter)
    (rov : rule_or_verif) =
  let arg_type = if definition then "T_irdata *" else "" in
  let tname, ret_type =
    match rov.rov_code with
    | Rule _ -> ("regle", "int ")
    | Verif _ -> ("verif", "void ")
  in
  let ret_type = if definition then ret_type else "" in
  Format.fprintf oc "%s%s_%s(%sirdata)%s@\n" ret_type tname
    (Pos.unmark rov.rov_name) arg_type
    (if definition then "" else ";")

let generate_rov_function (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rov : rule_or_verif) =
  let decl, ret =
    let noprint _ _ = () in
    match rov.rov_code with
    | Rule _ -> (noprint, fun fmt () -> Format.fprintf fmt "@ return 0;")
    | Verif _ ->
        ( (fun fmt () -> Format.fprintf fmt "int cond_def;@ double cond;@;"),
          noprint )
  in
  Format.fprintf oc "%a@[<v 2>{@ %a%a%a@]@;}@\n"
    (generate_rov_function_header ~definition:true)
    rov decl ()
    (generate_stmts (dgfip_flags : Dgfip_options.flags) program var_indexes)
    (Bir.rule_or_verif_as_statements rov)
    ret ()

let generate_rov_functions (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rovs : rule_or_verif list) =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rov_function
       (dgfip_flags : Dgfip_options.flags)
       program var_indexes)
    oc rovs

let generate_main_function_signature (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_extracted(T_irdata* irdata)%s"
    (if add_semicolon then ";" else "")

let _generate_main_function_signature_and_var_decls (oc : Format.formatter) () =
  Format.fprintf oc "%a {@\n@[<h 4>    @\n" generate_main_function_signature
    false;
  Format.fprintf oc "int cond_def;@\ndouble cond;@\n@\n";
  Format.fprintf oc
    {|
  #ifdef ANOMALY_LIMIT
  int anomaly_count = 0;
  int max_anomalies = ANOMALY_LIMIT;
  #endif /* ANOMALY_LIMIT */
|}

let _generate_return (oc : Format.formatter) () =
  Format.fprintf oc "@\nreturn 0;@]@\n}\n"

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc
    {|
/* %s */

#ifndef IR_HEADER_
#define IR_HEADER_

#include <stdio.h>

#include "irdata.h"
#include "const.h"
#include "var.h"

double my_var1;

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|}
    Prelude.message

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

let _generate_get_input_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_input_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_input_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_input_name_from_index(int index)%s"
    (if add_semicolon then ";\n" else "")

let generate_get_input_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_inputs()%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_input_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n}@\n@\n"
    generate_get_input_num_prototype false (List.length input_vars)

let generate_get_output_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_output_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_output_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output var %%s not found!\\n\", name);@\n\
     exit(-1);@]@\n\
     }@\n\
     @\n"
    generate_get_output_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (strcmp(\"%s\", name) == 0) { return %d; }"
           (Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)
           i))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_output_name_from_index(int index)%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_output_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output index %%d not found!\\n\", index);@\n\
     exit(-1);@]@\n\
     }@\n\
     @\n"
    generate_get_output_name_from_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (index == %d) { return \"%s\"; }" i
           (Pos.unmark (Bir.var_to_mir var).Mir.Variable.name)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_outputs()%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_output_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n}@\n@\n"
    generate_get_output_num_prototype false (List.length output_vars)

let _error_table_definitions (oc : Format.formatter) (program : program) =
  let error_set_size =
    Mir.VariableMap.cardinal program.mir_program.program_conds
  in
  Format.fprintf oc "typedef m_error_occurrence error_occurrences[%d];\n"
    error_set_size;
  Format.fprintf oc "typedef m_error errors[%d];\n" error_set_size

let print_error_line (oc : Format.formatter) (cond_data : Mir.condition_data) =
  let err, var = cond_data.cond_error in
  Format.fprintf oc
    "{.kind = \"%s\", .major_code = \"%s\", .minor_code = \"%s\", .isisf = \
     \"%s\",.description = \"%s\", .code_information = %s},@,"
    (Strings.sanitize_str err.descr.kind)
    (Strings.sanitize_str err.descr.major_code)
    (Strings.sanitize_str err.descr.minor_code)
    (Strings.sanitize_str err.descr.isisf)
    (Strings.sanitize_str err.descr.description)
    (match var with
    | None -> "\"\""
    | Some v -> (
        match v.alias with
        | Some alias -> "\"" ^ alias ^ "\""
        | None -> assert false))

let _generate_errors_table (oc : Format.formatter) (program : program) =
  Format.fprintf oc "@[<hv 2>static const errors m_errors = {@,%a@]};"
    (fun oc conds ->
      Mir.VariableMap.iter
        (fun _ cond_data -> print_error_line oc cond_data)
        conds)
    program.mir_program.program_conds

let generate_get_error_count_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_errors()%s"
    (if add_semicolon then ";\n\n" else "")

let _generate_get_error_count_func (oc : Format.formatter) (program : program) =
  Format.fprintf oc {|
%a {
  return %d;
}

|}
    generate_get_error_count_prototype false
    (Mir.VariableMap.cardinal program.mir_program.program_conds)

let generate_mpp_function_protoype (add_semicolon : bool) (return_type : bool)
    (oc : Format.formatter) (function_name : Bir.function_name) =
  let ret_type = if return_type then "struct S_discord *" else "void" in
  Format.fprintf oc "%s %s(T_irdata* irdata)%s" ret_type function_name
    (if add_semicolon then ";" else "")

let generate_mpp_function (dgfip_flags : Dgfip_options.flags)
    (program : Bir.program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) ((f, ret_type) : Bir.function_name * bool) =
  let { mppf_stmts; mppf_is_verif } =
    Bir.FunctionMap.find f program.mpp_functions
  in
  Format.fprintf oc "@[<hv 4>%a{@,int cond_def;@,double cond;@,%a%s@]}@,"
    (generate_mpp_function_protoype false mppf_is_verif)
    f
    (generate_stmts dgfip_flags program var_indexes)
    mppf_stmts
    (if ret_type then
     {|#ifdef FLG_MULTITHREAD
      return irdata->discords;
#else
      return discords;
#endif
|}
    else "")

let generate_mpp_functions (dgfip_flags : Dgfip_options.flags)
    (program : Bir.program) (oc : Format.formatter)
    (var_indexes : Dgfip_varid.var_id_map) =
  let funcs = Bir.FunctionMap.bindings program.Bir.mpp_functions in
  List.iter
    (fun (fname, { mppf_is_verif; _ }) ->
      generate_mpp_function
        (dgfip_flags : Dgfip_options.flags)
        program var_indexes oc (fname, mppf_is_verif))
    funcs

let generate_mpp_functions_signatures (oc : Format.formatter)
    (program : Bir.program) =
  let funcs = Bir.FunctionMap.bindings program.Bir.mpp_functions in
  Format.fprintf oc "@[<v 0>%a@]@,"
    (Format.pp_print_list (fun ppf (func, { mppf_is_verif; _ }) ->
         generate_mpp_function_protoype true mppf_is_verif ppf func))
    funcs

let generate_rovs_files (dgfip_flags : Dgfip_options.flags) (program : program)
    (vm : Dgfip_varid.var_id_map) =
  let module StringMap = Map.Make (String) in
  let default_file = "default" in
  let filemap =
    ROVMap.fold
      (fun _rov_id rov filemap ->
        let file =
          let pos = Pos.get_position rov.rov_name in
          if pos = Pos.no_pos then default_file
          else
            (Pos.get_file pos |> Filename.basename |> Filename.remove_extension)
            ^ ".c"
        in
        let filerovs =
          match StringMap.find_opt file filemap with
          | None -> []
          | Some fr -> fr
        in
        StringMap.add file (rov :: filerovs) filemap)
      program.rules_and_verifs StringMap.empty
  in
  StringMap.fold
    (fun file rovs orphan ->
      if String.equal file default_file then rovs @ orphan
      else
        let oc = open_out file in
        let fmt = Format.formatter_of_out_channel oc in
        Format.fprintf fmt
          {|
#include <math.h>
#include <stdio.h>
#include "var.h"

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#define _fmax(x,y) fmax((x),(y))
#define _fmin(x,y) fmin((x),(y))
#else
double _fmax(double x, double y);
double _fmin(double x, double y);
#endif
|};
        generate_rov_functions dgfip_flags program vm fmt rovs;
        Format.pp_print_flush fmt ();
        close_out oc;
        orphan)
    filemap []

let generate_implem_header oc header_filename =
  Format.fprintf oc
    {|
/* %s */

#include <string.h>
#include "enchain_static.c.inc"

#include "%s"

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
#define _fmax(x,y) fmax((x),(y))
#define _fmin(x,y) fmin((x),(y))
#else
double _fmax(double x, double y)
{ return (x > y) ? x : y; }
double _fmin(double x, double y)
{ return (x < y) ? x : y; }
#endif

|}
    Prelude.message header_filename

let generate_c_program (dgfip_flags: Dgfip_options.flags) (program : program)
    (_function_spec : Bir_interface.bir_function) (filename : string)
    (vm : Dgfip_varid.var_id_map) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let orphan_rovs = generate_rovs_files dgfip_flags program vm in
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a\n"
    generate_header ()
    (* error_table_definitions program  *)
    (* generate_get_input_index_prototype true *)
    (* generate_get_input_num_prototype true *)
    (* generate_get_input_name_from_index_prototype true *)
    (* generate_get_output_index_prototype true *)
    (* generate_get_output_name_from_index_prototype true *)
    (* generate_get_output_num_prototype true  *)
    (* generate_get_error_count_prototype true  *)
    generate_mpp_functions_signatures program
    (* generate_main_function_signature true  *)
    generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a\n"
    generate_implem_header header_filename
    (* generate_errors_table program *)
    (* generate_get_error_count_func program  *)
    (* generate_get_input_num_func function_spec *)
    (* generate_get_output_index_func function_spec *)
    (* generate_get_output_name_from_index_func function_spec *)
    (* generate_get_output_num_func function_spec *)
    (generate_rov_functions dgfip_flags program vm) orphan_rovs
    (generate_mpp_functions dgfip_flags program) vm
    (* generate_main_function_signature_and_var_decls ()
     * (generate_stmt program vm)
     *   (Bir.SFunctionCall (program.Bir.main_function, []), Pos.no_pos)
     * generate_return () *) ;
  close_out _oc[@@ocamlformat "disable"]
