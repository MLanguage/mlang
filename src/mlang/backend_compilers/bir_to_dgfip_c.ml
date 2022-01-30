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

open Mir

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
  | GetValueVar of string
  | PassPointer
  | None

let generate_variable (vm : Dgfip_varid.var_id_map) (offset : offset)
    ?(def_flag = false) (var : Variable.t) : string =
  try
    match offset with
    | PassPointer -> Dgfip_varid.gen_access_pointer vm var
    | _ ->
        let offset =
          match offset with
          | None -> ""
          | GetValueVar offset -> " + " ^ offset
          | GetValueConst offset -> " + " ^ string_of_int offset
          | PassPointer -> assert false
        in
        if def_flag then Dgfip_varid.gen_access_def vm var offset
        else Dgfip_varid.gen_access_val vm var offset
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark var.Mir.Variable.name))

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

let rec generate_c_expr (e : expression Pos.marked)
    (var_indexes : Dgfip_varid.var_id_map) : expression_composition =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_op, comp_op = generate_comp_op (Pos.unmark op) in
      let def_test =
        Format.sprintf "(%s %s %s)" se1.def_test def_op se2.def_test
      in
      let value_comp =
        Format.sprintf "(%s %s %s)" se1.value_comp comp_op se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | Binop ((Mast.Div, _), e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = Format.asprintf "(%s && %s)" se1.def_test se2.def_test in
      let value_comp =
        Format.asprintf "((%s==0.) ? (%s / %s) : 0.)" se2.value_comp
          se1.value_comp se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_op, comp_op = generate_binop (Pos.unmark op) in
      let def_test =
        Format.asprintf "(%s %s %s)" se1.def_test def_op se2.def_test
      in
      let value_comp =
        Format.asprintf "(%s %s %s)" se1.value_comp comp_op se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | Unop (op, e) ->
      let se = generate_c_expr e var_indexes in
      let def_test = se.def_test in
      let value_comp =
        Format.asprintf "(%s%s)" (generate_unop op) se.value_comp
      in
      { def_test; value_comp; locals = se.locals }
  | Index (var, e) ->
      let idx = generate_c_expr e var_indexes in
      let size = Option.get (Pos.unmark var).Mir.Variable.is_table in
      let idx_var = fresh_c_local "idx" in
      let def_test =
        Format.asprintf "(%s_d || %s >= %d)" idx_var idx_var size
      in
      let value_comp =
        Format.asprintf "((%s < 0.) ? 0. : (%s[(int)%s]))" idx_var
          (generate_variable var_indexes PassPointer (Pos.unmark var))
          idx_var
      in
      { def_test; value_comp; locals = [ (idx_var, idx) ] }
  | Conditional (c, t, f) ->
      let cond = generate_c_expr c var_indexes in
      let thenval = generate_c_expr t var_indexes in
      let elseval = generate_c_expr f var_indexes in
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
      let se = generate_c_expr arg var_indexes in
      let def_test = "1" in
      let value_comp = Format.sprintf "(%s ? 1. : 0.)" se.def_test in
      { def_test; value_comp; locals = se.locals }
  | FunctionCall (NullFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = "1" in
      let value_comp = Format.sprintf "(%s ? 0. : 1.)" se.def_test in
      { def_test; value_comp; locals = se.locals }
  | FunctionCall (ArrFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Format.sprintf "(my_arr(%s))" se.value_comp in
      { def_test; value_comp; locals = se.locals }
  | FunctionCall (InfFunc, [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = Format.sprintf "(my_floor(%s))" se.value_comp in
      { def_test; value_comp; locals = se.locals }
  | FunctionCall (MaxFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = "1" in
      let value_comp =
        Format.sprintf "(fmax(%s, %s))" se1.value_comp se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | FunctionCall (MinFunc, [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = "1" in
      let value_comp =
        Format.sprintf "(fmin(%s, %s))" se1.value_comp se2.value_comp
      in
      { def_test; value_comp; locals = se1.locals @ se2.locals }
  | FunctionCall (Multimax, [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr e1 var_indexes in
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
        value_comp = generate_variable var_indexes None var;
        locals = [];
      }
  | LocalVar lvar ->
      {
        def_test = "1";
        value_comp = "mlocal" ^ string_of_int lvar.Mir.LocalVariable.id;
        locals = [];
      }
  | GenericTableIndex ->
      { def_test = "1"; value_comp = "generic_index"; locals = [] }
  | Error -> assert false (* should not happen *)
  | LocalLet (lvar, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
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

let generate_var_def (var_indexes : Dgfip_varid.var_id_map)
    (var : Mir.Variable.t) (data : Mir.variable_data) (oc : Format.formatter) :
    unit =
  match data.var_definition with
  | SimpleVar e ->
      let se = generate_c_expr e var_indexes in
      Format.fprintf oc "%a%s = %s;@\n%s = %s;@\n" format_local_vars_defs
        se.locals
        (generate_variable ~def_flag:true var_indexes None var)
        se.def_test
        (generate_variable var_indexes None var)
        se.value_comp
  | TableVar (_, IndexTable es) ->
      Format.fprintf oc "%a"
        (fun fmt ->
          IndexMap.iter (fun i v ->
              let sv = generate_c_expr v var_indexes in
              Format.fprintf fmt "%a%s = %s;@\n%s = %s;@\n"
                format_local_vars_defs sv.locals
                (generate_variable ~def_flag:true var_indexes (GetValueConst i)
                   var)
                sv.def_test
                (generate_variable var_indexes (GetValueConst i) var)
                sv.value_comp))
        es
  | TableVar (size, IndexGeneric e) ->
      let sv = generate_c_expr e var_indexes in
      Format.fprintf oc
        "for (int generic_index=0; generic_index < %d; generic_index++) {@\n\
        \ @[<h 4> %a%s = %s;@\n\
         %s = %s;@]@\n\
        \ }@\n"
        size format_local_vars_defs sv.locals
        (generate_variable ~def_flag:true var_indexes
           (GetValueVar "generic_index") var)
        sv.def_test
        (generate_variable var_indexes (GetValueVar "generic_index") var)
        sv.value_comp
  | InputVar -> assert false

let generate_var_cond (var_indexes : Dgfip_varid.var_id_map)
    (cond : condition_data) (oc : Format.formatter) =
  let scond = generate_c_expr cond.cond_expr var_indexes in
  Format.fprintf oc
    {|
    %acond_def = %s;
    cond = %s;
    if (cond_def)
    {
      add_erreur(irdata, &erreur_%s, %s);
    }
|}
    format_local_vars_defs scond.locals scond.def_test scond.value_comp
    (Pos.unmark (fst cond.cond_error).Mir.Error.name)
    (match snd cond.cond_error with
    | None -> "NULL"
    | Some v -> Pos.unmark v.Mir.Variable.name)

let rec generate_stmt (program : Bir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmt : Bir.stmt) =
  match Pos.unmark stmt with
  | Bir.SAssign (var, vdata) -> generate_var_def var_indexes var vdata oc
  | SConditional _ -> assert false (* not in dgfip trivial M++ *)
  | SVerif v -> generate_var_cond var_indexes v oc
  | SRuleCall r ->
      let rule = Bir.RuleMap.find r program.rules in
      generate_rule_function_header ~definition:false oc rule
  | SFunctionCall (f, _) -> Format.fprintf oc "%s(irdata);\n" f

and generate_stmts (program : Bir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmts : Bir.stmt list) =
  Format.pp_print_list (generate_stmt program var_indexes) oc stmts

and generate_rule_function_header ~(definition : bool) (oc : Format.formatter)
    (rule : Bir.rule) =
  let arg_type = if definition then "T_irdata *" else "" in
  let ret_type = if definition then "int " else "" in
  Format.fprintf oc "%sm_rule_%s(%sirdata)%s@\n" ret_type rule.rule_name
    arg_type
    (if definition then "" else ";")

let generate_rule_function (program : Bir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (rule : Bir.rule) =
  Format.fprintf oc "%a@[<v 2>{@ %a@;return 0;@]@;}@\n"
    (generate_rule_function_header ~definition:true)
    rule
    (generate_stmts program var_indexes)
    rule.rule_stmts

let generate_rule_functions (program : Bir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (rules : Bir.rule Bir.RuleMap.t) =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (generate_rule_function program var_indexes)
    oc
    (Bir.RuleMap.bindings rules |> List.map snd)

let generate_main_function_signature (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_extracted(T_irdata* irdata)%s"
    (if add_semicolon then ";" else "")

let generate_main_function_signature_and_var_decls (oc : Format.formatter) () =
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

let generate_return (oc : Format.formatter) () =
  Format.fprintf oc "@\nreturn 0;@]@\n}"

let generate_header (oc : Format.formatter) () : unit =
  Format.fprintf oc
    {|
// %s

#ifndef IR_HEADER_
#define IR_HEADER_
#include <stdio.h>
#include "irdata.h"
#include "const.h"
#include "var.h"
#include "enchain_static.c"

#ifndef FLG_MULTITHREAD
#define add_erreur(a,b,c) add_erreur(b,c)
#endif

// for predef maths
double my_var1;

|}
    Prelude.message

let generate_footer (oc : Format.formatter) () : unit =
  Format.fprintf oc "\n#endif /* IR_HEADER_ */"

let generate_get_input_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_input_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_input_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_input_name_from_index(int index)%s"
    (if add_semicolon then ";\n" else "")

let generate_get_input_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_inputs()%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_input_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let input_vars =
    List.map fst (VariableMap.bindings function_spec.func_variable_inputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n};@\n@\n"
    generate_get_input_num_prototype false (List.length input_vars)

let generate_get_output_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_get_output_index(char *name)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output var %%s not found!\\n\", name);@\n\
     exit(-1);@]@\n\
     };@\n\
     @\n"
    generate_get_output_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (strcmp(\"%s\", name) == 0) { return %d; }"
           (Pos.unmark var.Mir.Variable.name)
           i))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_name_from_index_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "char* m_get_output_name_from_index(int index)%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_name_from_index_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc
    "%a {@\n\
     @[<h 4>    %a@\n\
     printf(\"Output index %%d not found!\\n\", index);@\n\
     exit(-1);@]@\n\
     };@\n\
     @\n"
    generate_get_output_name_from_index_prototype false
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (var, i) ->
         Format.fprintf fmt "if (index == %d) { return \"%s\"; }" i
           (Pos.unmark var.Mir.Variable.name)))
    (List.mapi (fun i x -> (x, i)) output_vars)

let generate_get_output_num_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_outputs()%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_output_num_func (oc : Format.formatter)
    (function_spec : Bir_interface.bir_function) =
  let output_vars =
    List.map fst (VariableMap.bindings function_spec.func_outputs)
  in
  Format.fprintf oc "%a {@\n@[<h 4>    return %d;@]@\n};@\n@\n"
    generate_get_output_num_prototype false (List.length output_vars)

let error_table_definitions (oc : Format.formatter) (program : Bir.program) =
  let error_set_size = VariableMap.cardinal program.mir_program.program_conds in
  Format.fprintf oc "typedef m_error_occurrence error_occurrences[%d];\n"
    error_set_size;
  Format.fprintf oc "typedef m_error errors[%d];\n" error_set_size

let print_error_line (oc : Format.formatter) (cond_data : condition_data) =
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

let generate_errors_table (oc : Format.formatter) (program : Bir.program) =
  Format.fprintf oc "@[<hv 2>static const errors m_errors = {@,%a@]};"
    (fun oc conds ->
      VariableMap.iter (fun _ cond_data -> print_error_line oc cond_data) conds)
    program.mir_program.program_conds

let generate_get_error_count_prototype (oc : Format.formatter)
    (add_semicolon : bool) =
  Format.fprintf oc "int m_num_errors()%s"
    (if add_semicolon then ";\n\n" else "")

let generate_get_error_count_func (oc : Format.formatter)
    (program : Bir.program) =
  Format.fprintf oc {|
%a {
  return %d;
}

|}
    generate_get_error_count_prototype false
    (VariableMap.cardinal program.mir_program.program_conds)

let generate_mpp_function (program : Bir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (f : Bir.function_name) =
  let stmts = Bir.FunctionMap.find f program.mpp_functions in
  Format.fprintf oc "@[<hv 4>int %s(T_irdata* irdata) {@,m_value cond;@,%a@]}@,"
    f
    (generate_stmts program var_indexes)
    stmts

let generate_mpp_functions (program : Bir.program) (oc : Format.formatter)
    (var_indexes : Dgfip_varid.var_id_map) =
  List.iter
    (fun (fname, _) -> generate_mpp_function program var_indexes oc fname)
    (Bir.FunctionMap.bindings program.mpp_functions)

let generate_implem_header oc header_filename =
  Format.fprintf oc "// File generated by the Mlang compiler\n\n";
  Format.fprintf oc "#include \"%s\"\n\n" header_filename;
  Format.fprintf oc "#include <string.h>\n"

let generate_c_program (program : Bir.program)
    (function_spec : Bir_interface.bir_function) (filename : string)
    (vm : Dgfip_varid.var_id_map) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let header_filename = Filename.remove_extension filename ^ ".h" in
  let _oc = open_out header_filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a" 
    generate_header ()
    error_table_definitions program 
    generate_get_input_index_prototype true
    generate_get_input_num_prototype true
    generate_get_input_name_from_index_prototype true
    generate_get_output_index_prototype true
    generate_get_output_name_from_index_prototype true
    generate_get_output_num_prototype true 
    generate_get_error_count_prototype true 
    generate_main_function_signature true 
    generate_footer ();
  close_out _oc;
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a%a%a%a%a%a%a%a%a%a%a%a" 
    generate_implem_header header_filename
    generate_errors_table program 
    generate_get_error_count_func program 
    generate_get_input_num_func function_spec
    generate_get_output_index_func function_spec
    generate_get_output_name_from_index_func function_spec
    generate_get_output_num_func function_spec
    (generate_rule_functions program vm)
      program.rules 
    (generate_mpp_functions program) vm
    generate_main_function_signature_and_var_decls ()
    (generate_stmt program vm) 
      (Bir.SFunctionCall (program.Bir.main_function, []), Pos.no_pos) 
    generate_return ();
  close_out _oc[@@ocamlformat "disable"]
