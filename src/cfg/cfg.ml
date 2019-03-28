(** Main data structure for M analysis *)

module Variable = struct
  type t = {
    name: string Ast.marked;
    id: int;
  }
  [@@deriving show]

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var (name: string Ast.marked) : t = {
    name; id = fresh_id ()
  }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

module LocalVariable = struct
  type t = {
    id: int;
  }
  [@@deriving show]

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var () : t = {
    id = fresh_id ()
  }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

type table_index =
  | LiteralIndex of int
  | GenericIndex
  | SymbolIndex of Variable.t
[@@deriving show]

type literal =
  | Int of int
  | Float of float
  | Bool of bool
[@@deriving show]

type func =
  | SumFunc
  | AbsFunc
  | MinFunc
  | MaxFunc
  | GtzFunc
  | GtezFunc
  | NullFunc
  | ArrFunc
  | InfFunc
  | PresentFunc
[@@deriving show]

type expression =
  | Comparison of Ast.comp_op Ast.marked * expression Ast.marked * expression Ast.marked
  | Binop of Ast.binop Ast.marked * expression Ast.marked * expression Ast.marked
  | Unop of Ast.unop * expression Ast.marked
  | Index of Variable.t Ast.marked * table_index Ast.marked
  | Conditional of expression Ast.marked * expression Ast.marked * expression Ast.marked option
  | FunctionCall of func * expression Ast.marked list
  | Literal of literal
  | Var of Variable.t
  | LocalVar of LocalVariable.t
  | LocalLet of LocalVariable.t * expression Ast.marked * expression Ast.marked
[@@deriving show]

module VariableMap = Map.Make(Variable)

type variable_data = {
  var_expr: expression Ast.marked;
}
[@@deriving show]

type program = {
  variables: variable_data VariableMap.t;
}
