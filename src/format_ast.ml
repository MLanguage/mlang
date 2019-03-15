open Ast

let format_func_name (f:func_name) : string = match f with
  | Unknown s -> s

let format_variable_generic_name (v: variable_generic_name) : string =
  v.base

let format_variable (v: variable) : string = match v with
  | Normal v -> v
  | Generic v -> format_variable_generic_name v

let format_lvalue (lv: lvalue) : string =
  Printf.sprintf "%s%s" (format_variable lv.var) (match lv.index with
      | Some vi -> "[" ^ (format_variable vi) ^ "]"
      | None -> ""
    )

let format_literal (l:literal) : string = match l with
  | Variable v -> format_variable v
  | Int i -> string_of_int i
  | Float f -> string_of_float f

let format_table_index (i:table_index) : string = match i with
  | LiteralIndex i -> string_of_int i
  | GenericIndex -> "X"
  | SymbolIndex v -> format_variable v

let format_set_value (sv: set_value) : string = match sv with
  | VarValue v -> format_variable v
  | Interval (i1, i2) -> Printf.sprintf "%d..%d" i1 i2

let format_loop_variable_ranges ((v, vs): loop_variable) =
  Printf.sprintf "un %s dans %s"
    (format_variable v)
    (String.concat "," (List.map (fun sv -> format_set_value sv) vs))

let format_loop_variable_value_set ((v, vs): loop_variable) =
  Printf.sprintf "%s=%s"
    (format_variable v)
    (String.concat "," (List.map (fun sv -> format_set_value sv) vs))

let format_loop_variables (lvs: loop_variables) : string =
  Printf.sprintf "pour %s:" (match lvs with
      | Ranges vvs ->
        String.concat " et "
          (List.map (fun (v, vs) -> format_loop_variable_ranges (v,vs)) vvs)
      | ValueSets vvs ->
        String.concat ";"
          (List.map (fun (v, vs) -> format_loop_variable_value_set (v,vs)) vvs)
    )

let format_set_value (v:set_value) : string = match v with
  | VarValue v -> format_variable v
  | Interval (i1,i2) -> (string_of_int i1) ^ ".." ^ (string_of_int i2)

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

let rec format_expression (e: expression) : string = match e with
  | TestInSet (belong, e, values) ->
    Printf.sprintf "(%s %sdans %s)"
      (format_expression e)
      (if belong then "" else "non ")
      (String.concat ", " (List.map (fun value -> format_set_value value) values))
  | Comparison (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_expression e1)
      (format_comp_op op)
      (format_expression e2)
  | Binop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_expression e1)
      (format_binop op)
      (format_expression e2)
  | Unop (op, e) ->
    (format_unop op) ^ " " ^ (format_expression e)
  | Index (v, i) ->
    Printf.sprintf "%s[%s]" (format_variable v) (format_table_index i)
  | Conditional (e1, e2, e3) ->
    Printf.sprintf "(si %s alors %s %sfinsi)"
      (format_expression e1)
      (format_expression e2)
      (match e3 with
       | None -> ""
       | Some e3 -> (format_expression e3)^ " ")
  | FunctionCall (f, args) ->
    Printf.sprintf "%s(%s)" (format_func_name f)
      (format_func_args args)
  | Literal l -> format_literal l
  | Loop (lvs, e) ->
    Printf.sprintf "%s%s"
      (format_loop_variables lvs)
      (format_expression e)

and format_func_args (args:func_args) : string = match args with
  | ArgList args -> String.concat ", "
                      (List.map (fun arg -> format_expression arg) args)
  | LoopList () -> "[...]"

let format_formula_decl (f:formula_decl) : string =
  Printf.sprintf "%s%s = %s"
    (format_lvalue f.lvalue)
    (match f.index with None -> "" | Some i ->
        "[" ^ format_table_index i ^ "]")
    (format_expression f.formula)

let format_formula (f:formula) : string = match f with
  | SingleFormula f -> format_formula_decl f
  | MultipleFormulaes (lvs, f) ->
    Printf.sprintf "%s\n%s"
      (format_loop_variables lvs)
      (format_formula_decl f)

let format_rule (r: rule) : string =
  Printf.sprintf "regle %s:\napplication %s;\n%s\n"
    (String.concat " " r.name)
    (String.concat ", " r.applications)
    (String.concat ";\n" (List.map (fun f -> format_formula f) r.formulaes))

let format_source_file_item (i:source_file_item) : string = match i with
  | Rule r -> format_rule r
  | _ -> "[...]"

let format_source_file (f: source_file) : string =
  String.concat "\n" (List.map (fun i -> format_source_file_item i) f)
