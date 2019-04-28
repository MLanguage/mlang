(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
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
knowledge of the CeCILL-C license and that you accept its terms.
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
module LocalVariableMap = Map.Make(LocalVariable)

module IndexMap = Map.Make(struct type t = int let compare = compare end)

type index_def =
  | IndexTable of (expression Ast.marked) IndexMap.t
  | IndexGeneric of expression Ast.marked

type variable_def =
  | SimpleVar of expression Ast.marked
  | TableVar of int * index_def
  | InputVar

type io =
  | Input
  | Output
  | Regular

type variable_data = {
  var_definition: variable_def;
  var_typ : typ option;
  var_io: io;
}


type program = variable_data VariableMap.t
