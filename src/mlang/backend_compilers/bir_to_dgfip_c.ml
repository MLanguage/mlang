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

(* Returns def test operator with the operator itself *)
let generate_comp_op (op : Mast.comp_op) : string * string =
  match op with
  | Mast.Gt -> ("&&", ">")
  | Mast.Gte -> ("&&", ">=")
  | Mast.Lt -> ("&&", "<")
  | Mast.Lte -> ("&&", "<=")
  | Mast.Eq -> ("&&", "==")
  | Mast.Neq -> ("&&", "!=")

let generate_binop (op : Mast.binop) : string * string =
  match op with
  | Mast.And -> ("&&", "&&")
  | Mast.Or -> ("||", "||")
  | Mast.Add -> ("||", "+")
  | Mast.Sub -> ("||", "-")
  | Mast.Mul -> ("&&", "*")
  | Mast.Div -> assert false
(* needs special case for division by zero *)

let generate_unop (op : Mast.unop) : string =
  match op with Mast.Not -> "!" | Mast.Minus -> "-"

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
    | PassPointer -> Dgfip_varid.gen_access_pointer vm mvar
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
          let vn = Pos.unmark mvar.Mir.Variable.name in
          let pos_tgv = Dgfip_varid.gen_access_pos_from_start vm mvar in
          if debug_flag then
            Format.asprintf "(aff3(\"%s\",irdata, %s), %s)" vn pos_tgv
              access_val
          else access_val
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark mvar.Mir.Variable.name))

type expression_composition = {
  def_test : string;
  value_comp : string;
  locals : (string * expression_composition) list;
}

let fresh_c_local : string -> string =
  let c = ref 0 in
  fun name ->
    let v = name ^ "_l" ^ string_of_int !c in
    incr c;
    v

let build_transitive_undef (e : expression_composition) : expression_composition
    =
  let expr_v = fresh_c_local "expr" in
  let def_test = expr_v ^ "_d" in
  {
    def_test;
    value_comp = Format.sprintf "((%s) ? %s : 0.)" def_test expr_v;
    locals = [ (expr_v, e) ];
  }

let rec generate_c_expr (dgfip_flags : Dgfip_options.flags)
    (e : expression Pos.marked) (var_indexes : Dgfip_varid.var_id_map) :
    expression_composition =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr dgfip_flags e1 var_indexes in
      let se2 = generate_c_expr dgfip_flags e2 var_indexes in
      let def_op, comp_op = generate_comp_op (Pos.unmark op) in
      let def_test =
        Format.sprintf "(%s %s %s)" se1.def_test def_op se2.def_test
      in
      let value_comp =
        Format.sprintf "(%s %s %s)" se1.value_comp comp_op se2.value_comp
      in
      build_transitive_undef
        { def_test; value_comp; locals = se1.locals @ se2.locals }
  | Binop ((Mast.Div, _), e1, e2) ->
      let se1 = generate_c_expr dgfip_flags e1 var_indexes in
      let se2 = generate_c_expr dgfip_flags e2 var_indexes in
      let def_test = Format.asprintf "(%s && %s)" se1.def_test se2.def_test in
      let value_comp =
        Format.asprintf "((%s==0.) ? 0. : (%s / %s))" se2.value_comp
          se1.value_comp se2.value_comp
      in
      build_transitive_undef
        { def_test; value_comp; locals = se1.locals @ se2.locals }
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr dgfip_flags e1 var_indexes in
      let se2 = generate_c_expr dgfip_flags e2 var_indexes in
      let def_op, comp_op = generate_binop (Pos.unmark op) in
      let def_test =
        Format.asprintf "(%s %s %s)" se1.def_test def_op se2.def_test
      in
      let value_comp =
        Format.asprintf "(%s %s %s)" se1.value_comp comp_op se2.value_comp
      in
      build_transitive_undef
        { def_test; value_comp; locals = se1.locals @ se2.locals }
  | Unop (op, e) ->
      let se = generate_c_expr dgfip_flags e var_indexes in
      let def_test = se.def_test in
      let value_comp =
        Format.asprintf "(%s%s)" (generate_unop op) se.value_comp
      in
      build_transitive_undef { def_test; value_comp; locals = se.locals }
  | Index (var, e) ->
      let idx = generate_c_expr dgfip_flags e var_indexes in
      let size =
        Option.get (Bir.var_to_mir (Pos.unmark var)).Mir.Variable.is_table
      in
      let idx_var = fresh_c_local "idx" in
      let def_test =
        Format.asprintf "(%s_d || %s >= %d)" idx_var idx_var size
      in
      let value_comp =
        Format.asprintf "((%s < 0.) ? 0. : (%s[(int)%s]))" idx_var
          (generate_variable var_indexes PassPointer (Pos.unmark var))
          idx_var
      in
      build_transitive_undef
        { def_test; value_comp; locals = [ (idx_var, idx) ] }
  | Conditional (c, t, f) ->
      let cond = generate_c_expr dgfip_flags c var_indexes in
      let thenval = generate_c_expr dgfip_flags t var_indexes in
      let elseval = generate_c_expr dgfip_flags f var_indexes in
      let cond_var = fresh_c_local "cond" in
      let then_var = fresh_c_local "then" in
      let else_var = fresh_c_local "else" in
      let def_test =
        Format.sprintf "(%s_d && (%s ? %s_d : %s_d))" cond_var cond_var then_var
          else_var
      in
      let value_comp =
        Format.sprintf "(%s_d ? (%s ? %s : %s) : 0.)" cond_var cond_var then_var
          else_var
      in
      {
        def_test;
        value_comp;
        locals = [ (cond_var, cond); (then_var, thenval); (else_var, elseval) ];
      }
  | FunctionCall (PresentFunc, [ arg ]) ->
      let se = generate_c_expr dgfip_flags arg var_indexes in
      let def_test = "1" in
      let value_comp = Format.sprintf "%s" se.def_test in
      { def_test; value_comp; locals = se.locals }
  | FunctionCall (NullFunc, [ arg ]) ->
      let se = generate_c_expr dgfip_flags arg var_indexes in
      let def_test = se.def_test in
      let value_comp =
        Format.sprintf "(%s ? (%s == 0 ? 1. : 0.) : 0.)" se.def_test
          se.value_comp
      in
      build_transitive_undef { def_test; value_comp; locals = se.locals }
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se = generate_c_expr dgfip_flags arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Format.sprintf "(my_arr(%s))" se.value_comp in
      build_transitive_undef { def_test; value_comp; locals = se.locals }
  | FunctionCall (InfFunc, [ arg ]) ->
      let se = generate_c_expr dgfip_flags arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Format.sprintf "(my_floor(%s))" se.value_comp in
      build_transitive_undef { def_test; value_comp; locals = se.locals }
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr dgfip_flags e1 var_indexes in
      let se2 = generate_c_expr dgfip_flags e2 var_indexes in
      let def_test = "1" in
      let value_comp =
        Format.sprintf "(_fmax(%s, %s))" se1.value_comp se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr dgfip_flags e1 var_indexes in
      let se2 = generate_c_expr dgfip_flags e2 var_indexes in
      let def_test = "1" in
      let value_comp =
        Format.sprintf "(_fmin(%s, %s))" se1.value_comp se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr dgfip_flags e1 var_indexes in
      let bound_var = fresh_c_local "bound" in
      let def_test = "1." in
      let value_comp =
        Format.asprintf "(multimax(%s, %s))" bound_var
          (generate_variable var_indexes PassPointer v2)
      in
      { def_test; value_comp; locals = [ (bound_var, bound) ] }
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) ->
      { def_test = "1"; value_comp = string_of_float f; locals = [] }
  | Literal Undefined -> { def_test = "0"; value_comp = "0."; locals = [] }
  | Var var ->
      {
        def_test = generate_variable ~def_flag:true var_indexes None var;
        value_comp =
          generate_variable var_indexes ~debug_flag:dgfip_flags.flg_trace None
            var;
        locals = [];
      }
  | LocalVar lvar ->
      {
        def_test = "mlocal" ^ string_of_int lvar.Mir.LocalVariable.id ^ "_d";
        value_comp = "mlocal" ^ string_of_int lvar.Mir.LocalVariable.id;
        locals = [];
      }
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let se1 = generate_c_expr dgfip_flags e1 var_indexes in
      let se2 = generate_c_expr dgfip_flags e2 var_indexes in
      let local_var = "mlocal" ^ string_of_int lvar.Mir.LocalVariable.id in
      let def_test = se2.def_test in
      let value_comp = se2.value_comp in
      { def_test; value_comp; locals = (local_var, se1) :: se2.locals }

let rec format_local_vars_defs (fmt : Format.formatter)
    (defs : (string * expression_composition) list) : unit =
  List.iter
    (fun (lvar, se) ->
      Format.fprintf fmt "%aint %s_d = %s;@\ndouble %s = %s;@\n"
        format_local_vars_defs se.locals lvar se.def_test lvar se.value_comp)
    defs

let generate_var_def (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : variable)
    (data : variable_data) (oc : Format.formatter) : unit =
  match data.var_definition with
  | SimpleVar e ->
      let se = generate_c_expr dgfip_flags e var_indexes in
      Format.fprintf oc "%a%s = %s;@\n%s = %s;@\n%s" format_local_vars_defs
        se.locals
        (generate_variable ~def_flag:true var_indexes None var)
        se.def_test
        (generate_variable var_indexes None var)
        se.value_comp
        (if dgfip_flags.flg_trace then
         let var = Bir.var_to_mir var in
         Format.asprintf "aff2(\"%s\", irdata, %s);@\n"
           (Pos.unmark var.Mir.Variable.name)
           (Dgfip_varid.gen_access_pos_from_start var_indexes var)
        else "")
      (* Format.fprintf oc "printf(\"%s = %%d/%%f\\n\", %s, %s);"
       *   (Pos.unmark (Bir.var_to_mir var).name)
       *   (generate_variable ~def_flag:true var_indexes None var)
       *   (generate_variable var_indexes None var) *)
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          Mir.IndexMap.iter (fun i v ->
              let sv = generate_c_expr dgfip_flags v var_indexes in
              Format.fprintf fmt "@[<hov 2>{@;%a%s = %s;@\n%s = %s;@\n@]@,}@;"
                format_local_vars_defs sv.locals
                (generate_variable ~def_flag:true ~debug_flag:false var_indexes
                   (GetValueConst i) var)
                sv.def_test
                (generate_variable var_indexes (GetValueConst i) var)
                sv.value_comp))
        es
  | TableVar (_size, IndexGeneric (v, e)) ->
      (* TODO: boundary checks *)
      let sv = generate_c_expr dgfip_flags e var_indexes in
      Format.fprintf oc "if(%s)@[<hov 2>{%a%s = %s;@ %s = %s;@]@;}@\n"
        (generate_variable var_indexes None ~def_flag:true ~debug_flag:false v)
        format_local_vars_defs sv.locals
        (generate_variable ~def_flag:true var_indexes (GetValueVar v) var)
        sv.def_test
        (generate_variable var_indexes (GetValueVar v) var)
        sv.value_comp
  | InputVar -> assert false

let generate_var_cond (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (cond : condition_data)
    (oc : Format.formatter) =
  let scond = generate_c_expr dgfip_flags cond.cond_expr var_indexes in
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
    %acond_def = %s;
    cond = %s;
    if (cond_def && (cond != 0.0))
    {
      add_erreur(irdata, &erreur_%s, %s);
    }
|}
    format_local_vars_defs scond.locals scond.def_test scond.value_comp erreur
    code

let rec generate_stmt (dgfip_flags : Dgfip_options.flags) (program : program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter) (stmt : stmt)
    =
  Format.fprintf oc "@[<hov 2>{@;";
  (match Pos.unmark stmt with
  | SAssign (var, vdata) ->
      generate_var_def dgfip_flags var_indexes var vdata oc
  | SConditional (cond, iftrue, iffalse) ->
      let cond_d = fresh_c_local "cond_d_mpp_" in
      let cond_v = fresh_c_local "cond_mpp_" in
      let cond =
        generate_c_expr dgfip_flags (Pos.same_pos_as cond stmt) var_indexes
      in
      Format.fprintf oc "%a@[<hov 2>%s = %s;@]@;@[<hov 2>%s = %s;@]@;"
        format_local_vars_defs cond.locals cond_d cond.def_test cond_v
        cond.value_comp;
      Format.fprintf oc "@[<hv 2>if(%s && %s){@,%a@]@,}@;" cond_d cond_v
        (generate_stmts dgfip_flags program var_indexes)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<hv 2>else if(%s){@,%a@]@,}@;" cond_d
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
  let funcs =
    Bir.FunctionMap.bindings
      (Bir_interface.context_agnostic_mpp_functions program)
  in
  List.iter
    (fun (fname, { mppf_is_verif; _ }) ->
      generate_mpp_function
        (dgfip_flags : Dgfip_options.flags)
        program var_indexes oc (fname, mppf_is_verif))
    funcs

let generate_mpp_functions_signatures (oc : Format.formatter)
    (program : Bir.program) =
  let funcs =
    Bir.FunctionMap.bindings
      (Bir_interface.context_agnostic_mpp_functions program)
  in
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
