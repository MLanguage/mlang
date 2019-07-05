(*
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>
*)


(* A type with decidable equality, only requirement
   for the union_find data structure *)
module type Type =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type S =
sig
  type elt
  type t

  val create : elt -> t
  val find : t -> elt
  val union : t -> t -> unit
end

(* A functor with one argument to construct a union-find
   structure. *)
module Make (Eq: Type) =
struct
  type elt = Eq.t
  (* A node of the disjoint set structure.
     The rank and parent fields are mutable to enable path compression
     and rank updates *)
  type t = Node of {
      mutable rank : int ;
      mutable parent : t ;
      elem : elt
    }

  (* Creates a new union_find node, referencing
     itself as its parent. *)
  let create elm =
    (* `let rec ...` required here to reference ourselves *)
    let rec node = Node {
        rank = 0;
        parent = node;
        elem = elm
      }
    in node

  (* Do a find() on a given node, searching
     for its representative *)
  let rec find' (Node inst)  =
    if inst.parent != (Node inst) then
      (* Path-compression here *)
      let _ = inst.parent <- find' (inst.parent) in
      inst.parent
    else
      inst.parent

  let find (Node inst) =
    let Node n = (find' (Node inst)) in
    n.elem

  (* Union function, performs union by rank and
     uses find() for path compression *)
  let union node_a node_b =
    if node_a == node_b then ()
    else
      let (Node root_a) = find' node_a
      and (Node root_b) = find' node_b in
      if (root_a.rank > root_b.rank) then
        let _ = root_b.parent <- (Node root_a)
        in ()
      else if (root_b.rank < root_a.rank) then
        let _ = root_a.parent <- (Node root_b)
        in ()
      else
      if root_b.elem < root_a.elem then
        let _ = root_a.parent <- (Node root_b)
        and _ = root_b.rank <- root_b.rank + 1
        in ()
      else
        let _ = root_b.parent <- (Node root_a)
        and _ = root_a.rank <- root_a.rank + 1
        in ()
end
