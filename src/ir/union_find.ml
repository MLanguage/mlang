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
