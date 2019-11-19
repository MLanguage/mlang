(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Mvg

let generate_variable (var:Variable.t) : string =
  let v = match var.alias with Some v -> v | None -> Pos.unmark var.Variable.name in
  let v = String.lowercase_ascii v in
  let v =
    if same_execution_number var.Variable.execution_number
        (Ast_to_mvg.dummy_exec_number (Pos.get_position var.Variable.name))
    then v else
      Printf.sprintf "%s_%d_%d" v
        (var.Variable.execution_number.Mvg.rule_number)
        (var.Variable.execution_number.Mvg.seq_number)
  in
  if Re.Str.string_match (Re.Str.regexp "[0-9].+") v 0 then
    "var_" ^ v
  else
    v

let generate_name (v:Variable.t) : string =
  match v.alias with Some v -> v | None -> Pos.unmark v.Variable.name

let generate_comp_op (op: Ast.comp_op) : string = match op with
  | Ast.Gt -> "gt"
  | Ast.Gte -> "gte"
  | Ast.Lt -> "lt"
  | Ast.Lte -> "lte"
  | Ast.Eq -> "eq"
  | Ast.Neq -> "neq"

let generate_binop (op: Ast.binop) : string = match op with
  | Ast.And -> "and"
  | Ast.Or -> "or"
  | Ast.Add -> "add"
  | Ast.Sub -> "sub"
  | Ast.Mul -> "mul"
  | Ast.Div -> "div"

(*
  Since there is no way to have inline let bindings, we have to collect all local
  variables created...
*)
let rec generate_java_expr (e: expression Pos.marked) (scc: unit VariableMap.t) : string * string list =
  match Pos.unmark e with
  | Comparison (op, e1, e2) ->
    let s1, ls1 = generate_java_expr e1 scc in
    let s2, ls2 = generate_java_expr e2 scc in
    Printf.sprintf "%s.%s(%s)" s1 (generate_comp_op (Pos.unmark op)) s2,
    ls1@ls2
  | Binop (op, e1, e2) ->
    let s1, ls1 = generate_java_expr e1 scc in
    let s2, ls2 = generate_java_expr e2 scc in
    Printf.sprintf "%s.%s(%s)" s1 (generate_binop (Pos.unmark op)) s2,
    ls1@ls2
  | Unop (Ast.Minus, e) ->
    let s, ls = generate_java_expr e scc in
    Printf.sprintf "%s.minus()" s,
    ls
  | Unop (Ast.Not, e) ->
    let s, ls = generate_java_expr e scc in
    Printf.sprintf "%s.not()" s,
    ls
  | Index _ ->
    assert false (* unimplemented *)
  | Conditional (((LocalVar _, _) as e1), e2, e3) ->
    let s1, ls1 = generate_java_expr e1 scc in
    let s2, ls2 = generate_java_expr e2 scc in
    let s3, ls3 = generate_java_expr e3 scc in
    let v = LocalVariable.new_var () in
    let s_def =
      Printf.sprintf "(%s.is_undefined() ? new MValue() : (%s.get_bool_value() ? %s : %s))"
        s1 s1 s2 s3
    in
    let s = Printf.sprintf "        MValue v%d = %s;" (v.LocalVariable.id) s_def in
    Printf.sprintf "v%d" v.LocalVariable.id,
    ls1@ls2@ls3@[s] (* be careful, this is wrong is any of these has side effects ! *)
  | Conditional (e1, e2, e3) ->
    let v1 = LocalVariable.new_var () in
    let new_e = Pos.same_pos_as (LocalLet (
        v1, e1,
        Pos.same_pos_as (Conditional (
            (Pos.same_pos_as (LocalVar v1) e1),
            e2,
            e3
          )) e
      )) e
    in generate_java_expr new_e scc
  | FunctionCall (PresentFunc, [arg]) ->
    let sarg, lsarg = generate_java_expr arg scc in
    Printf.sprintf "new MValue(!%s.is_undefined())" sarg,
    lsarg
  | FunctionCall (NullFunc, [arg]) ->
    let sarg, lsarg = generate_java_expr arg scc in
    Printf.sprintf "new MValue(%s.is_undefined())" sarg,
    lsarg
  | FunctionCall (ArrFunc, [arg]) ->
    let sarg, lsarg = generate_java_expr arg scc in
    Printf.sprintf "%s.round()" sarg,
    lsarg
  | FunctionCall (InfFunc, [arg]) ->
    let sarg, lsarg = generate_java_expr arg scc in
    Printf.sprintf "%s.floor()" sarg,
    lsarg
  | FunctionCall _ -> assert false (* should not happen *)
  | Literal (Bool true) -> "true", []
  | Literal (Bool false) -> "false", []
  | Literal (Int i) ->
    Printf.sprintf "new MValue(%d.0)" i, []
  | Literal (Float f) ->
    Printf.sprintf "new MValue(%f)" f, []
  | Literal Undefined ->
    "new MValue()", []
  | Var var ->
    "this." ^ generate_variable var, []
  | LocalVar lvar -> Printf.sprintf "v%d" lvar.LocalVariable.id, []
  | GenericTableIndex -> assert false (* unimplemented *)
  | Error -> assert false (* TODO *)
  | LocalLet (lvar, e1, e2) ->
    let s1, ls1 = generate_java_expr e1 scc in
    let s2, ls2 = generate_java_expr e2 scc in
    s2,
    ls1@[
      Printf.sprintf "        MValue v%d = %s;"  lvar.LocalVariable.id s1
    ]@ls2


let generate_var_def
    (program : program)
    (var: Variable.t)
    (scc: unit VariableMap.t)
    (oc: out_channel) : unit =
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then begin
      Printf.fprintf oc "    // %s: %s\n"
        (generate_name var)
        (Pos.unmark var.Variable.descr);
      match data.var_definition with
      | SimpleVar e ->
        Printf.fprintf oc "    // Defined %s\n"
          (Pos.format_position (Pos.get_position e));
        let s, ls = generate_java_expr e scc in
        Printf.fprintf oc "    void compute_%s() {\n"
          (generate_variable var);
        List.iter (fun s ->
            Printf.fprintf oc "%s\n" s;
          ) ls;
        Printf.fprintf oc "        this.%s = %s;\n"
          (generate_variable var)
          s;
        Printf.fprintf oc "    }\n\n"
      | TableVar (_, _) ->
        assert false (* unimplemented *)
      | InputVar -> assert false (* should not happen *)
    end
  with
  | Not_found ->
    ()

let generate_var_call
    (program : program)
    (var: Variable.t)
    (scc: unit VariableMap.t)
    (oc: out_channel) : unit =
  try
    let data = VariableMap.find var program.program_vars in
    if data.var_io = Regular || data.var_io = Output then begin
      match data.var_definition with
      | SimpleVar _ ->
        Printf.fprintf oc "        this.compute_%s();\n"
          (generate_variable var);
      | TableVar (_, _) ->
        assert false (* unimplemented *)
      | InputVar -> assert false (* should not happen *)
    end
  with
  | Not_found ->
    let cond = VariableMap.find var program.program_conds in
    let s, ls = generate_java_expr cond.cond_expr scc in
    let fresh = LocalVariable.new_var () in
    let cond_name = Printf.sprintf "cond%d"   fresh.LocalVariable.id in
    List.iter (fun s ->
        Printf.fprintf oc "%s\n" s;
      ) ls;
    Printf.fprintf oc
      "        // Verification condition %s\n        MValue %s = %s;\n        if ((!%s.is_undefined()) && %s.get_bool_value()) {\n            throw new MError(\"Error triggered\\n%s\");\n        }\n"
      (Pos.format_position (Pos.get_position cond.cond_expr))
      cond_name
      s
      cond_name
      cond_name
      (String.concat "\\n"
         (List.map
            (fun err ->
               Printf.sprintf "%s: %s"
                 (Pos.unmark err.Error.name)
                 (Pos.unmark err.Error.descr)
            )
            cond.cond_errors
         )
      )


let generate_java_program
    (program: program)
    (filename : string)
    (_: int)
  : unit =
  let oc = open_out filename in
  let exec_order = Execution_order.get_execution_order program in
  let all_vars =
    List.rev
      (List.map
         (fun (var, _) -> var)
         (VariableMap.bindings program.program_vars)
      )
  in
  let input_vars =
    List.rev
      (List.map
         (fun (var, _) -> var)
         (List.filter
            (fun (_, data) -> data.var_io = Input)
            (VariableMap.bindings program.program_vars)
         ))
  in
  let output_vars =
    List.map
      (fun (var, _) -> var)
      (List.filter
         (fun (_, data) -> data.var_io = Output)
         (VariableMap.bindings program.program_vars)
      )
  in
  Printf.fprintf oc "// %s\n\n" Prelude.message;
  Printf.fprintf oc "\

class MError extends Exception {
    public MError(String s) {
      super(s);
    }
}

class MValue {
    double value;
    boolean bool_value;
    boolean is_undefined;
    public MValue(double value) {
        this.value = value;
        this.is_undefined = false;
        this.bool_value = false;
    }

    public MValue(boolean value) {
        this.value = 0.0;
        this.is_undefined = false;
        this.bool_value = value;
    }

    public double get_value() {
        return this.value;
    }

    public boolean get_bool_value() {
        return this.bool_value;
    }

    public boolean is_undefined() {
        return this.is_undefined;
    }

    public MValue() {
        this.value = 0.0;
        this.is_undefined = true;
        this.bool_value = false;
    }

    public MValue add(MValue rhs) {
        if (this.is_undefined && rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value + rhs.value);
    }

    public MValue sub(MValue rhs) {
        if (this.is_undefined && rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value - rhs.value);
    }

    public MValue mul(MValue rhs) {
        if (this.is_undefined && rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value * rhs.value);
    }

    public MValue minus() {
        if (this.is_undefined) {
            return new MValue();
        }
        return new MValue(- this.value);
    }

    public MValue not() {
        if (this.is_undefined) {
            return new MValue();
        }
        return new MValue(!this.bool_value);
    }

    public MValue div(MValue rhs) {
        if (rhs.is_undefined) {
            return new MValue();
        }
        if (this.is_undefined) {
            return new MValue(0.0);
        }
        if (rhs.value == 0.0) {
            return new MValue();
        }
        return new MValue(this.value / rhs.value);
    }

    public MValue round() {
        if (this.is_undefined) {
            return new MValue();
        } else {
            return new MValue(Math.round(this.value));
        }
    }

    public MValue floor() {
        if (this.is_undefined) {
            return new MValue();
        } else {
            return new MValue(Math.floor(this.value));
        }
    }

    public MValue and(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.bool_value && rhs.bool_value);
    }

    public MValue or(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.bool_value || rhs.bool_value);
    }

    public MValue lt(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value < rhs.value);
    }

    public MValue lte(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value <= rhs.value);
    }

    public MValue gt(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value > rhs.value);
    }

    public MValue gte(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value >= rhs.value);
    }

    public MValue eq(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value == rhs.value);
    }

    public MValue neq(MValue rhs) {
        if (this.is_undefined || rhs.is_undefined) {
            return new MValue();
        }
        return new MValue(this.value != rhs.value);
    }
}

class IR {
    // Internal variables
%s

    // Constructor
    public IR(java.util.Map<String, MValue> input_values) {
%s
    }
"
    (String.concat
       "\n"
       (List.map
          (fun var ->
             Printf.sprintf "    MValue %s;"
               (generate_variable var)
          )
          all_vars
       )
    )
    (String.concat
       "\n"
       (List.map
          (fun var ->
             Printf.sprintf "       this.%s = input_values.get(\"%s\");"
               (generate_variable var)
               (generate_name var)
          )
          input_vars
       )
    );
  List.iter (fun scc ->
      let in_scc = VariableMap.cardinal scc > 1 in
      if in_scc then begin
        assert false (* unimplemented *)
      end;
      VariableMap.iter (fun var _ ->
          generate_var_def program var scc oc
        ) scc;
    ) exec_order;
  Printf.fprintf oc "\

    // Main tax computation. Call before any output getter.
    public void compute() throws MError {
";
  List.iter (fun scc ->
      let in_scc = VariableMap.cardinal scc > 1 in
      if in_scc then begin
        assert false (* unimplemented *)
      end;
      VariableMap.iter (fun var _ ->
          generate_var_call program var scc oc
        ) scc;
    ) exec_order;
  Printf.fprintf oc "    }\n\n";
  Printf.fprintf oc "%s" begin
    String.concat "\n    " begin List.map begin fun var ->
        Printf.sprintf "\

    // Returning output %s
    public MValue get%s() {
        return this.%s;
    }
"
          (generate_name var)
          (generate_name var)
          (generate_variable var)
      end output_vars
    end
  end;
  Printf.fprintf oc "}";
  close_out oc
