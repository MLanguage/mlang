(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.
*)

(** AST pretty_printer *)

open Ast

let format_position (pos: position) : string =
  let (s, e) = pos.pos_loc in
  Printf.sprintf "in file %s, from %d:%d to %d:%d"
    pos.pos_filename
    s.Lexing.pos_lnum (s.Lexing.pos_cnum - s.Lexing.pos_bol + 1)
    e.Lexing.pos_lnum (e.Lexing.pos_cnum - e.Lexing.pos_bol + 1)

let format_application (app: application) : string  =
  app

let format_chaining (c: chaining) : string =
  c

let format_rule_name (rn : rule_name) : string =
  String.concat " " (List.map unmark rn)

let format_variable_name (v: variable_name) : string =
  v

let format_func_name (f:func_name) : string = f

let format_variable_generic_name (v: variable_generic_name) : string =
  v.base

let format_variable (v: variable) : string = match v with
  | Normal v -> format_variable_name v
  | Generic v -> format_variable_generic_name v

let format_verification_name (n: verification_name) : string =
  String.concat " " (List.map unmark n)

let format_error_name (e:error_name) : string =
  e

let format_literal (l:literal) : string = match l with
  | Variable v -> format_variable v
  | Int i -> string_of_int i
  | Float f -> string_of_float f

let format_table_index (i:table_index) : string = match i with
  | LiteralIndex i -> string_of_int i
  | GenericIndex -> "X"
  | SymbolIndex v -> format_variable v

let format_lvalue (lv: lvalue) : string =
  Printf.sprintf "%s%s" (format_variable (Ast.unmark lv.var)) (match lv.index with
      | Some vi -> "[" ^ (format_table_index (unmark vi)) ^ "]"
      | None -> ""
    )

let format_set_value (sv: set_value) : string = match sv with
  | VarValue v -> format_variable (unmark v)
  | Interval (i1, i2) -> Printf.sprintf "%d..%d" (unmark i1) (unmark i2)
  | IntValue i -> string_of_int (Ast.unmark i)

let format_set_value_loop (sv: set_value_loop) : string = match sv with
  | VarParam v -> Printf.sprintf "%s" (unmark v)
  | IntervalLoop (i1, i2) -> Printf.sprintf "%d..%d" (unmark i1) (unmark i2)

let format_comp_op (op: comp_op) : string = match op with
  | Gt -> ">"
  | Gte -> ">="
  | Lt -> "<"
  | Lte -> "<="
  | Eq -> "="
  | Neq -> "!="

let format_binop (op: binop) : string = match op with
  | And -> "et"
  | Or -> "ou"
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let format_unop (op: unop) : string = match op with
  | Not -> "non"
  | Minus -> "-"

let format_loop_variable_ranges ((v, vs): loop_variable) =
  Printf.sprintf "un %c dans %s"
    (unmark v)
    (String.concat "," (List.map (fun sv -> format_set_value_loop sv) vs))

let format_loop_variable_value_set ((v, vs): loop_variable) =
  Printf.sprintf "%c=%s"
    (unmark v)
    (String.concat "," (List.map (fun sv -> format_set_value_loop sv) vs))

let format_loop_variables (lvs: loop_variables) : string =
  Printf.sprintf "%s:" (match lvs with
      | Ranges vvs ->
        String.concat " et "
          (List.map (fun (v, vs) -> format_loop_variable_ranges (v,vs)) vvs)
      | ValueSets vvs ->
        String.concat ";"
          (List.map (fun (v, vs) -> format_loop_variable_value_set (v,vs)) vvs)
    )

let rec format_expression (e: expression) : string = match e with
  | TestInSet (belong, e, values) ->
    Printf.sprintf "(%s %sdans %s)"
      (format_expression (unmark e))
      (if belong then "" else "non ")
      (String.concat ", " (List.map (fun value -> format_set_value value) values))
  | Comparison (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_expression (unmark e1))
      (format_comp_op (unmark op))
      (format_expression (unmark e2))
  | Binop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_expression (unmark e1))
      (format_binop (unmark op))
      (format_expression (unmark e2))
  | Unop (op, e) ->
    (format_unop op) ^ " " ^ (format_expression (unmark e))
  | Index (v, i) ->
    Printf.sprintf "%s[%s]"
      (format_variable (unmark v))
      (format_table_index (unmark i))
  | Conditional (e1, e2, e3) ->
    Printf.sprintf "(si %s alors %s %sfinsi)"
      (format_expression (unmark e1))
      (format_expression (unmark e2))
      (match e3 with
       | None -> ""
       | Some e3 -> (format_expression (unmark e3))^ " ")
  | FunctionCall (f, args) ->
    Printf.sprintf "%s(%s)" (format_func_name (Ast.unmark f))
      (format_func_args args)
  | Literal l -> format_literal l
  | Loop (lvs, e) ->
    Printf.sprintf "pour %s%s"
      (format_loop_variables (unmark lvs))
      (format_expression (unmark e))

and format_func_args (args:func_args) : string = match args with
  | ArgList args -> String.concat ", "
                      (List.map (fun arg -> format_expression (unmark arg)) args)
  | LoopList (lvs, e) ->
    Printf.sprintf "%s%s"
      (format_loop_variables (unmark lvs))
      (format_expression (unmark e))

let format_formula_decl (f:formula_decl) : string =
  Printf.sprintf "%s = %s"
    (format_lvalue (unmark f.lvalue))
    (format_expression (unmark f.formula))

let format_formula (f:formula) : string = match f with
  | SingleFormula f -> format_formula_decl f
  | MultipleFormulaes (lvs, f) ->
    Printf.sprintf "pour %s\n%s"
      (format_loop_variables (unmark lvs))
      (format_formula_decl f)

let format_rule (r: rule) : string =
  Printf.sprintf "regle %s:\napplication %s;\n%s;\n"
    (String.concat " " (List.map unmark r.rule_name))
    (String.concat ", " (List.map unmark r.rule_applications))
    (String.concat ";\n" (List.map (fun f -> format_formula (unmark f)) r.rule_formulaes))

let format_computed_typ (t:computed_typ) : string = match t with
  | Base -> "base"
  | GivenBack -> "restituee"

let format_input_variable_subtype (t: input_variable_subtype) : string = match t with
  | Context -> "contexte"
  | Family -> "famille"
  | Penality -> "penalite"
  | Income -> "revenu"

let format_value_typ (t:value_typ) : string = match t with
  | Boolean -> "BOOLEEN"
  | DateYear -> "DATE_AAAA"
  | DateDayMonthYear -> "DATE_JJMMAAAA"
  | DateMonth -> "DATE_MM"
  | Integer -> "ENTIER"
  | Real -> "REEL"

let format_input_attribute ((n,v): input_variable_attribute marked * literal marked) : string =
  Printf.sprintf "%s = %s"
    (unmark n)
    (format_literal (unmark v))

let format_input_variable (v:input_variable) : string =
  Printf.sprintf "%s saisie %s %s%s %s : %s%s;"
    (format_variable_name (unmark v.input_name))
    (format_input_variable_subtype (unmark v.input_subtyp))
    (String.concat " " (List.map format_input_attribute v.input_attributes))
    (if v.input_given_back then " restituee" else "")
    (format_variable_name (unmark v.input_alias))
    (unmark v.input_description)
    (match v.input_typ with
     | None -> ""
     | Some t ->format_value_typ (unmark t))

let format_computed_variable (v: computed_variable) : string =
  Printf.sprintf "%s%s calculee %s : %s%s;"
    (unmark v.comp_name)
    (match v.comp_table with
     | None -> ""
     | Some t -> " " ^ (string_of_int (unmark t)))
    (String.concat " " (List.map (fun st -> format_computed_typ (unmark st)) v.comp_subtyp))
    (match v.comp_typ with
     | None -> ""
     | Some t -> " " ^ (format_value_typ (unmark t)))
    (unmark v.comp_description)

let format_variable_decl (v: variable_decl) : string = match v with
  | ComputedVar v -> format_computed_variable (unmark v)
  | ConstVar (name, value)->
    Printf.sprintf "%s : const = %s"
      (format_variable_name (unmark name))
      (format_literal (unmark value))
  | InputVar v -> format_input_variable (unmark v)

let format_verification_condition (vc: verification_condition) : string =
  Printf.sprintf "si %s\n alors erreur %s;"
    (format_expression (unmark vc.verif_cond_expr))
    (String.concat " " (List.map (fun n -> format_error_name (unmark n)) vc.verif_cond_errors))

let format_verification (v: verification) : string =
  Printf.sprintf "verif %s : %s;\n%s"
    (format_verification_name v.verif_name)
    (String.concat " " (List.map (fun app -> format_application (unmark app)) v.verif_applications))
    (String.concat "\n" (List.map (fun vc -> format_verification_condition (unmark vc)) v.verif_conditions))

let format_error_typ (e:error_typ) : string = match e with
  | Anomaly -> "anomalie"
  | Discordance -> "discordance"
  | Information -> "information"

let format_error_ (e:error_) : string =
  Printf.sprintf "%s : %s : %s;"
    (format_error_name (unmark e.error_name))
    (format_error_typ (unmark e.error_typ))
    (String.concat " : " (List.map (fun d -> (unmark d)) e.error_descr))

let format_source_file_item (i:source_file_item) : string = match i with
  | Application app -> Printf.sprintf "application %s;" (format_application (unmark app))
  | Chaining (c, apps) ->
    Printf.sprintf "enchaineur %s %s;"
      (format_chaining c)
      (String.concat " " (List.map (fun app -> format_application (unmark app)) apps))
  | Variable vd -> format_variable_decl vd
  | Rule r -> format_rule r
  | Verification v -> format_verification v
  | Error e -> format_error_ e
  | Output o -> Printf.sprintf "sortie(%s);" (format_variable_name (unmark o))

let format_source_file (f: source_file) : string =
  String.concat "\n" (List.map (fun i -> format_source_file_item (unmark i)) f)
