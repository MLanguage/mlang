(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-B
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
knowledge of the CeCILL-B license and that you accept its terms.
*)

module type EqType =
sig
  type t
  val equal : t -> t -> bool
end

module type S =
sig
  type elt
  type t

  val create : elt -> t
  val find: t -> elt
  val get_elt: t -> elt
  val union : t -> t -> unit
end

module Make (Eq: EqType) =
struct
  type elt = Eq.t
  type t = Node of {
      mutable rank : int ;
      mutable parent : t ;
      elem : elt
    }

  let create elm =
    let rec node = Node {
        rank = 0;
        parent = node;
        elem = elm
      }
    in node

  let rec find_aux (Node inst) =
    if inst.parent != (Node inst) then
      let _ = inst.parent <- find_aux (inst.parent) in
      inst.parent
    else
      inst.parent

  let find n =
    let (Node r) = find_aux n in
    r.elem

  let get_elt (Node inst) = inst.elem

  let union node_a node_b =
    if node_a == node_b then ()
    else
      let (Node root_a) = find_aux node_a
      and (Node root_b) = find_aux node_b in
      if (root_a.rank > root_b.rank) then
        let _ = root_b.parent <- (Node root_a)
        in ()
      else if (root_b.rank < root_a.rank) then
        let _ = root_a.parent <- (Node root_b)
        in ()
      else
        let _ = root_a.parent <- (Node root_b)
        and _ = root_b.rank <- root_b.rank + 1
        in ()
end
