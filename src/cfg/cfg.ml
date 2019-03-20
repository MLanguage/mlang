module Variable = struct
  type t = {
    name: string;
    id: int;
  }

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var (name: string) : t = {
    name; id = fresh_id ()
  }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

module Function = struct
  type t = {
    name: string;
    id: int;
  }

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var (name: string) : t = {
    name; id = fresh_id ()
  }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

module FunctionMap = Map.Make(Function)

type table_index =
  | LiteralIndex of int
  | GenericIndex
  | SymbolIndex of Variable.t

type literal =
  | Int of int
  | Float of float

type expression =
  | Comparison of Ast.comp_op Ast.marked * expression Ast.marked * expression Ast.marked
  | Binop of Ast.binop Ast.marked * expression Ast.marked * expression Ast.marked
  | Unop of Ast.unop * expression Ast.marked
  | Index of Variable.t Ast.marked * table_index Ast.marked
  | Conditional of expression Ast.marked * expression Ast.marked * expression Ast.marked option
  | FunctionCall of Function.t * expression Ast.marked list
  | Literal of literal
  | Var of Variable.t

module VariableMap = Map.Make(Variable)

type variable_data = {
  var_expr: expression Ast.marked;
}

type function_data = unit

type program = {
  variables: variable_data VariableMap.t;
  functions: function_data FunctionMap.t
}
