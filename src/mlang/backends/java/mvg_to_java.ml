(* Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

open Mvg

let generate_variable (var : Variable.t) : string =
  let v = match var.alias with Some v -> v | None -> Pos.unmark var.Variable.name in
  let v = String.lowercase_ascii v in
  let v =
    if
      same_execution_number var.Variable.execution_number
        (Ast_to_mvg.dummy_exec_number (Pos.get_position var.Variable.name))
    then v
    else
      Format.asprintf "%s_%d_%d" v var.Variable.execution_number.Mvg.rule_number
        var.Variable.execution_number.Mvg.seq_number
  in
  if Re.Str.string_match (Re.Str.regexp "[0-9].+") v 0 then "var_" ^ v else v

let generate_name (v : Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let generate_comp_op (op : Ast.comp_op) : string =
  match op with
  | Ast.Gt -> "gt"
  | Ast.Gte -> "gte"
  | Ast.Lt -> "lt"
  | Ast.Lte -> "lte"
  | Ast.Eq -> "eq"
  | Ast.Neq -> "neq"

let generate_binop (op : Ast.binop) : string =
  match op with
  | Ast.And -> "and"
  | Ast.Or -> "or"
  | Ast.Add -> "add"
  | Ast.Sub -> "sub"
  | Ast.Mul -> "mul"
  | Ast.Div -> "div"

(* Since there is no way to have inline let bindings, we have to collect all local variables
   created... *)
let rec generate_java_expr (e : expression Pos.marked) : string * string list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let s1, ls1 = generate_java_expr e1 in
      let s2, ls2 = generate_java_expr e2 in
      (Format.asprintf "%s.%s(%s)" s1 (generate_comp_op (Pos.unmark op)) s2, ls1 @ ls2)
  | Binop (op, e1, e2) ->
      let s1, ls1 = generate_java_expr e1 in
      let s2, ls2 = generate_java_expr e2 in
      (Format.asprintf "%s.%s(%s)" s1 (generate_binop (Pos.unmark op)) s2, ls1 @ ls2)
  | Unop (Ast.Minus, e) ->
      let s, ls = generate_java_expr e in
      (Format.asprintf "%s.minus()" s, ls)
  | Unop (Ast.Not, e) ->
      let s, ls = generate_java_expr e in
      (Format.asprintf "%s.not()" s, ls)
  | Index _ -> assert false (* unimplemented *)
  | Conditional (((LocalVar _, _) as e1), e2, e3) ->
      let s1, ls1 = generate_java_expr e1 in
      let s2, ls2 = generate_java_expr e2 in
      let s3, ls3 = generate_java_expr e3 in
      let v = LocalVariable.new_var () in
      let s_def =
        Format.asprintf "(%s.is_undefined() ? new MValue() : (%s.get_bool_value() ? %s : %s))" s1 s1
          s2 s3
      in
      let s = Format.asprintf "        MValue v%d = %s;" v.LocalVariable.id s_def in
      (Format.asprintf "v%d" v.LocalVariable.id, ls1 @ ls2 @ ls3 @ [ s ])
      (* be careful, this is wrong is any of these has side effects ! *)
  | Conditional (e1, e2, e3) ->
      let v1 = LocalVariable.new_var () in
      let new_e =
        Pos.same_pos_as
          (LocalLet
             (v1, e1, Pos.same_pos_as (Conditional (Pos.same_pos_as (LocalVar v1) e1, e2, e3)) e))
          e
      in
      generate_java_expr new_e
  | FunctionCall (PresentFunc, [ arg ]) ->
      let sarg, lsarg = generate_java_expr arg in
      (Format.asprintf "new MValue(!%s.is_undefined())" sarg, lsarg)
  | FunctionCall (NullFunc, [ arg ]) ->
      let sarg, lsarg = generate_java_expr arg in
      (Format.asprintf "new MValue(%s.is_undefined())" sarg, lsarg)
  | FunctionCall (ArrFunc, [ arg ]) ->
      let sarg, lsarg = generate_java_expr arg in
      (Format.asprintf "%s.round()" sarg, lsarg)
  | FunctionCall (InfFunc, [ arg ]) ->
      let sarg, lsarg = generate_java_expr arg in
      (Format.asprintf "%s.floor()" sarg, lsarg)
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> (Format.asprintf "new MValue(%f)" f, [])
  | Literal Undefined -> ("new MValue()", [])
  | Var var -> ("this." ^ generate_variable var, [])
  | LocalVar lvar -> (Format.asprintf "v%d" lvar.LocalVariable.id, [])
  | GenericTableIndex -> assert false (* unimplemented *)
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
      let s1, ls1 = generate_java_expr e1 in
      let s2, ls2 = generate_java_expr e2 in
      (s2, ls1 @ [ Format.asprintf "        MValue v%d = %s;" lvar.LocalVariable.id s1 ] @ ls2)

let generate_var_def (program : program) (var : Variable.t) (oc : Format.formatter) : unit =
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then begin
      Format.fprintf oc "    // %s: %s\n" (generate_name var) (Pos.unmark var.Variable.descr);
      match data.var_definition with
      | SimpleVar e ->
          Format.fprintf oc "    // Defined %a\n" Pos.format_position (Pos.get_position e);
          let s, ls = generate_java_expr e in
          Format.fprintf oc "    void compute_%s() {\n" (generate_variable var);
          List.iter (fun s -> Format.fprintf oc "%s\n" s) ls;
          Format.fprintf oc "        this.%s = %s;\n" (generate_variable var) s;
          Format.fprintf oc "    }\n\n"
      | TableVar (_, _) -> assert false (* unimplemented *)
      | InputVar -> assert false
      (* should not happen *)
    end
  with Not_found -> ()

let generate_var_call (program : program) (var : Variable.t) (oc : Format.formatter) : unit =
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then
      match data.var_definition with
      | SimpleVar _ -> Format.fprintf oc "        this.compute_%s();\n" (generate_variable var)
      | TableVar (_, _) -> assert false (* unimplemented *)
      | InputVar -> assert false
    (* should not happen *)
  with Not_found ->
    let cond = VariableMap.find var program.program_conds in
    let s, ls = generate_java_expr cond.cond_expr in
    let fresh = LocalVariable.new_var () in
    let cond_name = Format.asprintf "cond%d" fresh.LocalVariable.id in
    List.iter (fun s -> Format.fprintf oc "%s\n" s) ls;
    Format.fprintf oc
      "        // Verification condition %a\n\
      \        MValue %s = %s;\n\
      \        if ((!%s.is_undefined()) && %s.get_bool_value()) {\n\
      \            throw new MError(\"Error triggered\\n%s\");\n\
      \        }\n"
      Pos.format_position (Pos.get_position cond.cond_expr) cond_name s cond_name cond_name
      (String.concat "\\n"
         (List.map
            (fun err ->
              Format.asprintf "%s: %s" (Pos.unmark err.Error.name) (Pos.unmark err.Error.descr))
            cond.cond_errors))

let generate_java_program (program : program) (dep_graph : Dependency.DepGraph.t)
    (filename : string) : unit =
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  let exec_order = Execution_order.get_execution_order dep_graph in
  let all_vars =
    List.rev (List.map (fun (var, _) -> var) (VariableMap.bindings program.program_vars))
  in
  let input_vars =
    List.rev
      (List.map
         (fun (var, _) -> var)
         (List.filter
            (fun (_, data) -> data.var_io = Input)
            (VariableMap.bindings program.program_vars)))
  in
  let output_vars =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Output)
         (VariableMap.bindings program.program_vars))
  in
  Format.fprintf oc "// %s\n\n" Prelude.message;
  Format.fprintf oc
    "\n\
     class MError extends Exception {\n\
    \    public MError(String s) {\n\
    \      super(s);\n\
    \    }\n\
     }\n\n\
     class MValue {\n\
    \    double value;\n\
    \    boolean bool_value;\n\
    \    boolean is_undefined;\n\
    \    public MValue(double value) {\n\
    \        this.value = value;\n\
    \        this.is_undefined = false;\n\
    \        this.bool_value = false;\n\
    \    }\n\n\
    \    public MValue(boolean value) {\n\
    \        this.value = 0.0;\n\
    \        this.is_undefined = false;\n\
    \        this.bool_value = value;\n\
    \    }\n\n\
    \    public double get_value() {\n\
    \        return this.value;\n\
    \    }\n\n\
    \    public boolean get_bool_value() {\n\
    \        return this.bool_value;\n\
    \    }\n\n\
    \    public boolean is_undefined() {\n\
    \        return this.is_undefined;\n\
    \    }\n\n\
    \    public MValue() {\n\
    \        this.value = 0.0;\n\
    \        this.is_undefined = true;\n\
    \        this.bool_value = false;\n\
    \    }\n\n\
    \    public MValue add(MValue rhs) {\n\
    \        if (this.is_undefined && rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value + rhs.value);\n\
    \    }\n\n\
    \    public MValue sub(MValue rhs) {\n\
    \        if (this.is_undefined && rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value - rhs.value);\n\
    \    }\n\n\
    \    public MValue mul(MValue rhs) {\n\
    \        if (this.is_undefined && rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value * rhs.value);\n\
    \    }\n\n\
    \    public MValue minus() {\n\
    \        if (this.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(- this.value);\n\
    \    }\n\n\
    \    public MValue not() {\n\
    \        if (this.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(!this.bool_value);\n\
    \    }\n\n\
    \    public MValue div(MValue rhs) {\n\
    \        if (rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        if (this.is_undefined) {\n\
    \            return new MValue(0.0);\n\
    \        }\n\
    \        if (rhs.value == 0.0) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value / rhs.value);\n\
    \    }\n\n\
    \    public MValue round() {\n\
    \        if (this.is_undefined) {\n\
    \            return new MValue();\n\
    \        } else {\n\
    \            return new MValue(Math.round(this.value));\n\
    \        }\n\
    \    }\n\n\
    \    public MValue floor() {\n\
    \        if (this.is_undefined) {\n\
    \            return new MValue();\n\
    \        } else {\n\
    \            return new MValue(Math.floor(this.value));\n\
    \        }\n\
    \    }\n\n\
    \    public MValue and(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.bool_value && rhs.bool_value);\n\
    \    }\n\n\
    \    public MValue or(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.bool_value || rhs.bool_value);\n\
    \    }\n\n\
    \    public MValue lt(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value < rhs.value);\n\
    \    }\n\n\
    \    public MValue lte(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value <= rhs.value);\n\
    \    }\n\n\
    \    public MValue gt(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value > rhs.value);\n\
    \    }\n\n\
    \    public MValue gte(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value >= rhs.value);\n\
    \    }\n\n\
    \    public MValue eq(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value == rhs.value);\n\
    \    }\n\n\
    \    public MValue neq(MValue rhs) {\n\
    \        if (this.is_undefined || rhs.is_undefined) {\n\
    \            return new MValue();\n\
    \        }\n\
    \        return new MValue(this.value != rhs.value);\n\
    \    }\n\
     }\n\n\
     class IR {\n\
    \    // Internal variables\n\
     %s\n\n\
    \    // Constructor\n\
    \    public IR(java.util.Map<String, MValue> input_values) {\n\
     %s\n\
    \    }\n"
    (String.concat "\n"
       (List.map (fun var -> Format.asprintf "    MValue %s;" (generate_variable var)) all_vars))
    (String.concat "\n"
       (List.map
          (fun var ->
            Format.asprintf "       this.%s = input_values.get(\"%s\");" (generate_variable var)
              (generate_name var))
          input_vars));
  List.iter (fun var -> generate_var_def program var oc) exec_order;
  Format.fprintf oc
    "\n\
    \    // Main tax computation. Call before any output getter.\n\
    \    public void compute() throws MError {\n";
  List.iter (fun var -> generate_var_call program var oc) exec_order;
  Format.fprintf oc "    }\n\n";
  Format.fprintf oc "%s"
    (String.concat "\n    "
       (List.map
          begin
            fun var ->
            Format.asprintf
              "\n\
              \    // Returning output %s\n\
              \    public MValue get%s() {\n\
              \        return this.%s;\n\
              \    }\n"
              (generate_name var) (generate_name var) (generate_variable var)
          end
          output_vars));
  Format.fprintf oc "}";
  close_out _oc
