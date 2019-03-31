(*
  Copyright 2018 Denis Merigoux and INRIA

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

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

type typ =
  | Integer
  | Real
  | Boolean

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
  | Index of Variable.t Ast.marked * expression Ast.marked
  | Conditional of expression Ast.marked * expression Ast.marked * expression Ast.marked
  | FunctionCall of func * expression Ast.marked list
  | Literal of literal
  | Var of Variable.t
  | LocalVar of LocalVariable.t
  | GenericTableIndex
  | Error
  | LocalLet of LocalVariable.t * expression Ast.marked * expression Ast.marked
[@@deriving show]

module VariableMap = Map.Make(Variable)

module IndexMap = Map.Make(struct type t = int let compare = compare end)

type index_def =
  | IndexTable of (expression Ast.marked) IndexMap.t
  | IndexGeneric of expression Ast.marked

type variable_def =
  | SimpleVar of expression Ast.marked
  | TableVar of int * index_def
  | InputVar

type variable_data = {
  var_definition: variable_def;
  var_typ : typ option;
}


type program = variable_data VariableMap.t
