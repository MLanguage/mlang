(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

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

(**{1 Variables} *)

(** Variables are first-class objects *)
module Variable = struct
  type t = {
    name: string Ast.marked; (** The position is the variable declaration *)
    alias: string option; (** Input variable have an alias *)
    id: int; (** Each variable has an unique ID *)
    descr: string Ast.marked; (** Description taken from the variable declaration *)
  }
  [@@deriving show]

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var (name: string Ast.marked) (alias: string option) (descr: string Ast.marked) : t = {
    name; id = fresh_id (); descr; alias
  }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

(**
    Local variables don't appear in the M source program but can be introduced by let bindings when translating to MVG.
    They should be De Bruijn indices but instead are unique globals identifiers out of laziness.
*)
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

(** Type of MVG values *)
type typ =
  | Integer
  | Real
  | Boolean
[@@deriving show]

type literal =
  | Int of int
  | Float of float
  | Bool of bool
  | Undefined
[@@deriving show]

(** MVg only supports a restricted set of functions *)
type func =
  | SumFunc (** Sums the arguments *)
  | AbsFunc (** Absolute value *)
  | MinFunc (** Minimum of a list of values *)
  | MaxFunc (** Maximum of a list of values *)
  | GtzFunc (** Greater than zero (strict) ? *)
  | GtezFunc (** Greater or equal than zero ? *)
  | NullFunc (** Equal to zero ? *)
  | ArrFunc (** Round to nearest integer *)
  | InfFunc (** Truncate to integer *)
  | PresentFunc (** Different than zero ? *)
  | Multimax (** ??? *)
  | Supzero (** ??? *)
[@@deriving show]


(**
   MVG expressions are simpler than M; there are no loops or syntaxtic sugars. Because M lets you
   define conditional without an else branch although it is an expression-based language, we include
   an [Error] constructor to which the missing else branch is translated to.

   Because translating to MVG requires a lot of unrolling and expansion, we introduce a [LocalLet]
   construct to avoid code duplication.
*)


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

(**
   MVG programs are just mapping from variables to their definitions, and make a massive use
   of [VariableMap].
*)
module VariableMap =
struct
  include Map.Make(Variable)

  let show vprinter map = fold (fun k v acc ->
      Printf.sprintf "%s\n%s -> %s" acc (Variable.show k) (vprinter v)) map ""
end

module LocalVariableMap =
struct
  include Map.Make(LocalVariable)

  let show vprinter map = fold (fun k v acc ->
      Printf.sprintf "%s\n%s -> %s" acc (LocalVariable.show k) (vprinter v)) map ""
end

(**
   This map is used to store the definitions of all the cells of a table variable that is not
   not defined generically
*)
module IndexMap = Map.Make(struct type t = int let compare = compare end)

type index_def =
  | IndexTable of (expression Ast.marked) IndexMap.t
  | IndexGeneric of expression Ast.marked

(**
   The definitions here are modeled closely to the source M language. One could also adopt
   a more lambda-calculus-compatible model with functions used to model tables.
*)
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
  var_typ : typ option; (** The typing info here comes from the variable declaration in the source program *)
  var_io: io;
  var_is_undefined: bool;
}

(**{1 Verification conditions}*)

(** Errors are first-class objects *)
module Error = struct
  type t = {
    name: string Ast.marked; (** The position is the variable declaration *)
    id: int; (** Each variable has an unique ID *)
    descr: string Ast.marked; (** Description taken from the variable declaration *)
  }
  [@@deriving show]

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_error (name: string Ast.marked) (descr: string Ast.marked) : t = {
    name; id = fresh_id (); descr;
  }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

type condition_data = {
  cond_expr: expression Ast.marked;
  cond_errors: Error.t list;
}

type program = {
  program_vars: variable_data VariableMap.t;
  program_conds: condition_data VariableMap.t; (** Conditions are affected to dummy variables *)
}

(** {1 Helpers }*)


(** Throws an error in case of alias not found *)
let find_var_by_alias (p: program) (alias: string) : Variable.t =
  let v = VariableMap.fold (fun v _ acc ->
      match acc, v.Variable.alias with
      | (Some _, _) | (None, None ) -> acc
      | (None, Some v_alias) -> if v_alias = alias then Some v else None
    ) p.program_vars None in
  match v with
  | Some v -> v
  | None ->
    raise (Errors.TypeError (
        Errors.Variable (Printf.sprintf "alias not found (%s)" alias)
      ))
