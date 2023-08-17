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
module D = DecoupledExpr

let rec generate_c_expr (e : expression Pos.marked)
    (var_indexes : Dgfip_varid.var_id_map) : D.expression_composition =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let safe_def =
        match (Pos.unmark op, Pos.unmark e2) with
        | Mast.Gt, Mir.(Literal (Undefined | Float 0.)) ->
            (* hack to catch positive test in M *) true
        | _ -> false
      in
      let def_test = D.dand se1.def_test se2.def_test in
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
        D.comp op se1.value_comp se2.value_comp
      in
      D.build_transitive_composition ~safe_def { def_test; value_comp }
  | Binop ((Mast.Div, _), e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dand se1.def_test se2.def_test in
      let value_comp =
        D.ite se2.value_comp (D.div se1.value_comp se2.value_comp) (D.lit 0.)
      in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test =
        match Pos.unmark op with
        | Mast.And | Mast.Mul -> D.dand se1.def_test se2.def_test
        | Mast.Or | Mast.Add | Mast.Sub -> D.dor se1.def_test se2.def_test
        | Mast.Div -> assert false
        (* see above *)
      in
      let op e1 e2 =
        match Pos.unmark op with
        | Mast.And -> D.dand e1 e2
        | Mast.Or -> D.dor e1 e2
        | Mast.Add -> D.plus e1 e2
        | Mast.Sub -> D.sub e1 e2
        | Mast.Mul -> D.mult e1 e2
        | Mast.Div -> assert false
        (* see above *)
      in
      let value_comp = op se1.value_comp se2.value_comp in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | Unop (op, e) ->
      let se = generate_c_expr e var_indexes in
      let def_test = se.def_test in
      let op, safe_def =
        match op with
        | Mast.Not -> (D.dnot, false)
        | Mast.Minus -> (D.minus, true)
      in
      let value_comp = op se.value_comp in
      D.build_transitive_composition ~safe_def { def_test; value_comp }
  | Index (var, e) ->
      let idx = generate_c_expr e var_indexes in
      let size =
        Option.get (Bir.var_to_mir (Pos.unmark var)).Mir.Variable.is_table
      in
      let idx_var = D.new_local () in
      let def_test =
        D.let_local idx_var idx.value_comp
          (D.dand
             (D.dand idx.def_test
                (D.comp "<" (D.local_var idx_var) (D.lit (float_of_int size))))
             (D.access (Pos.unmark var) Def (D.local_var idx_var)))
      in
      let value_comp =
        D.let_local idx_var idx.value_comp
          (D.ite
             (D.comp "<" (D.local_var idx_var) (D.lit 0.))
             (D.lit 0.)
             (D.access (Pos.unmark var) Val (D.local_var idx_var)))
      in
      D.build_transitive_composition { def_test; value_comp }
  | Conditional (c, t, f) ->
      let cond = generate_c_expr c var_indexes in
      let thenval = generate_c_expr t var_indexes in
      let elseval = generate_c_expr f var_indexes in
      let def_test =
        D.dand cond.def_test
          (D.ite cond.value_comp thenval.def_test elseval.def_test)
      in
      let value_comp =
        D.ite cond.value_comp thenval.value_comp elseval.value_comp
      in
      D.build_transitive_composition { def_test; value_comp }
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = D.dtrue in
      let value_comp = se.def_test in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (NullFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dand def_test (D.comp "==" se.value_comp (D.lit 0.)) in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_arr" [ se.value_comp ] in
      (* Here we boldly assume that rounding value of `undef` will give zero,
         given the invariant. Pretty sure that not true, in case of doubt, turn
         `safe_def` to false *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (InfFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_floor" [ se.value_comp ] in
      (* same as above *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (AbsFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "fabs" [ se.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "max" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "min" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr e1 var_indexes in
      let def_test =
        D.dfun "multimax_def" [ bound.value_comp; D.m_var v2 PassPointer Def ]
      in
      let value_comp =
        D.dfun "multimax" [ bound.value_comp; D.m_var v2 PassPointer Val ]
      in
      D.build_transitive_composition { def_test; value_comp }
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> { def_test = D.dtrue; value_comp = D.lit f }
  | Literal Undefined -> { def_test = D.dfalse; value_comp = D.lit 0. }
  | Var var ->
      { def_test = D.m_var var None Def; value_comp = D.m_var var None Val }
  | LocalVar lvar ->
      let ldef, lval = D.locals_from_m lvar in
      { def_test = D.local_var ldef; value_comp = D.local_var lval }
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let ldef, lval = D.locals_from_m lvar in
      let declare_local constr =
        D.let_local ldef se1.def_test (D.let_local lval se1.value_comp constr)
      in
      {
        def_test = declare_local se2.def_test;
        value_comp = declare_local se2.value_comp;
      }

let generate_m_assign (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable) (offset : D.offset)
    (oc : Format.formatter) (se : D.expression_composition) : unit =
  let def_var = D.generate_variable ~def_flag:true var_indexes offset var in
  let val_var = D.generate_variable var_indexes offset var in
  let locals, def, value = D.build_expression se in
  if D.is_always_true def then
    Format.fprintf oc "%a%a@,@[<v 2>{@,%a@,@]}" D.format_local_declarations
      locals
      (D.format_assign dgfip_flags var_indexes def_var)
      def
      (D.format_assign dgfip_flags var_indexes val_var)
      value
  else
    Format.fprintf oc "%a%a@,@[<v 2>if(%s){@;%a@]@,}@,else %s = 0.;"
      D.format_local_declarations locals
      (D.format_assign dgfip_flags var_indexes def_var)
      def def_var
      (D.format_assign dgfip_flags var_indexes val_var)
      value val_var;
  if dgfip_flags.flg_trace then
    let var = Bir.var_to_mir var in
    Format.fprintf oc "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Mir.Variable.name)
      (Dgfip_varid.gen_access_pos_from_start var_indexes var)

let generate_var_def (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable) (def : variable_def)
    (fmt : Format.formatter) : unit =
  match def with
  | SimpleVar e ->
      let se = generate_c_expr e var_indexes in
      generate_m_assign dgfip_flags var_indexes var None fmt se
  | TableVar (_, IndexTable es) ->
      Mir.IndexMap.iter
        (fun i v ->
          let sv = generate_c_expr v var_indexes in
          Format.fprintf fmt "@[<hov 2>{@,%a@]}@,"
            (generate_m_assign dgfip_flags var_indexes var (GetValueConst i))
            sv)
        es
  | TableVar (_size, IndexGeneric (v, e)) ->
      (* TODO: boundary checks *)
      let sv = generate_c_expr e var_indexes in
      Format.fprintf fmt "if(%s)@[<hov 2>{%a@]@;}"
        (D.generate_variable var_indexes None ~def_flag:true v)
        (generate_m_assign dgfip_flags var_indexes var (GetValueVar v))
        sv
  | InputVar -> assert false

let generate_var_cond (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (cond : condition_data)
    (oc : Format.formatter) =
  let econd = generate_c_expr cond.cond_expr var_indexes in
  let locals, _def, value =
    D.build_expression
    @@ D.build_transitive_composition ~safe_def:true
         {
           def_test = D.dtrue;
           value_comp = D.dand econd.def_test econd.value_comp;
         }
  in
  let erreur = Pos.unmark (fst cond.cond_error).Mir.Error.name in
  let code =
    match snd cond.cond_error with
    | None -> "NULL"
    | Some v ->
        Format.sprintf "\"%s\""
          (Pos.unmark (Bir.var_to_mir v).Mir.Variable.name)
  in
  Format.fprintf oc "%a%a@,@[<v 2>if(cond){@," D.format_local_declarations
    locals
    (D.format_assign dgfip_flags var_indexes "cond")
    value;
  Format.fprintf oc "add_erreur(irdata, &erreur_%s, %s);@]@,}" erreur code

let fresh_c_local =
  let c = ref 0 in
  fun name ->
    let s = name ^ string_of_int !c in
    incr c;
    s

let rec generate_stmt (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter) (stmt : stmt)
    =
  match Pos.unmark stmt with
  | SAssign (var, vdata) ->
      Format.fprintf oc "@[<v 2>{@,";
      generate_var_def dgfip_flags var_indexes var vdata oc;
      Format.fprintf oc "@]@,}"
  | SConditional (cond, iftrue, iffalse) ->
      Format.fprintf oc "@[<v 2>{@,";
      let cond_val = fresh_c_local "mpp_cond" in
      let cond_def = cond_val ^ "_d" in
      let locals, def, value =
        D.build_expression
        @@ generate_c_expr (Pos.same_pos_as cond stmt) var_indexes
      in
      Format.fprintf oc "%a%a@;%a" D.format_local_declarations locals
        (D.format_assign dgfip_flags var_indexes cond_def)
        def
        (D.format_assign dgfip_flags var_indexes cond_val)
        value;
      Format.fprintf oc "@[<hov 2>if(%s && %s){@,%a@]@,}" cond_def cond_val
        (generate_stmts dgfip_flags program var_indexes)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<hov 2>else if(%s){@,%a@]@,}" cond_def
          (generate_stmts dgfip_flags program var_indexes)
          iffalse;
      Format.fprintf oc "@]@,}"
  | SVerif v -> generate_var_cond dgfip_flags var_indexes v oc
  | SRovCall r ->
      let rov = ROVMap.find r program.rules_and_verifs in
      generate_rov_function_header ~definition:false oc rov
  | SFunctionCall (f, _) -> Format.fprintf oc "%s(irdata);" f

and generate_stmts (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmts : stmt list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list (generate_stmt dgfip_flags program var_indexes) oc stmts;
  Format.fprintf oc "@]"

and generate_rov_function_header ~(definition : bool) (oc : Format.formatter)
    (rov : rule_or_verif) =
  let arg_type = if definition then "T_irdata *" else "" in
  let tname, ret_type =
    match rov.rov_code with
    | Rule _ -> ("regle", "int ")
    | Verif _ -> ("verif", "void ")
  in
  let ret_type = if definition then ret_type else "" in
  Format.fprintf oc "%s%s_%s(%sirdata)%s" ret_type tname
    (Pos.unmark rov.rov_name) arg_type
    (if definition then "" else ";")

let generate_rov_function (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rov : rule_or_verif) =
  let decl, ret =
    let noprint _ _ = () in
    match rov.rov_code with
    | Rule _ -> (noprint, fun fmt () -> Format.fprintf fmt "@,return 0;")
    | Verif _ ->
        ((fun fmt () -> Format.fprintf fmt "register int cond;@;"), noprint)
  in
  Format.fprintf oc "@[<v 2>%a{@,%a%a%a@]@,}"
    (generate_rov_function_header ~definition:true)
    rov decl ()
    (generate_stmts (dgfip_flags : Dgfip_options.flags) program var_indexes)
    (Bir.rule_or_verif_as_statements rov)
    ret ()

let generate_rov_functions (dgfip_flags : Dgfip_options.flags)
    (program : program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (rovs : rule_or_verif list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rov_function
       (dgfip_flags : Dgfip_options.flags)
       program var_indexes)
    oc rovs;
  Format.fprintf oc "@]"

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

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|}
    Prelude.message

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

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
  Format.fprintf oc "@[<v 2>%a{@,%a%s@]@,}@,"
    (generate_mpp_function_protoype false mppf_is_verif)
    f
    (generate_stmts dgfip_flags program var_indexes)
    mppf_stmts
    (if ret_type then
     {|
#ifdef FLG_MULTITHREAD
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
    (folder : string) (vm : Dgfip_varid.var_id_map) =
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
          match StrMap.find_opt file filemap with None -> [] | Some fr -> fr
        in
        StrMap.add file (rov :: filerovs) filemap)
      program.rules_and_verifs StrMap.empty
  in
  StrMap.fold
    (fun file rovs orphan ->
      if String.equal file default_file then rovs @ orphan
      else
        let oc = open_out (Filename.concat folder file) in
        let fmt = Format.formatter_of_out_channel oc in
        Format.fprintf fmt
          {|
#include <math.h>
#include <stdio.h>
#include "var.h"

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

|};
        generate_rov_functions dgfip_flags program vm fmt rovs;
        Format.fprintf fmt "@\n@.";
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


|}
    Prelude.message header_filename

let generate_c_program (dgfip_flags: Dgfip_options.flags) (program : program)
    (_function_spec : Bir_interface.bir_function) (filename : string)
    (vm : Dgfip_varid.var_id_map) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let folder = Filename.dirname filename in
  let orphan_rovs = generate_rovs_files dgfip_flags program folder vm in
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a@\n@."
    generate_header ()
    generate_mpp_functions_signatures program
    generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a@\n@."
    generate_implem_header (Filename.basename header_filename)
    (generate_rov_functions dgfip_flags program vm) orphan_rovs
    (generate_mpp_functions dgfip_flags program) vm;
  close_out _oc[@@ocamlformat "disable"]
