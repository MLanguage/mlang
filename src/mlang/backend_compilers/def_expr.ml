(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2026 DGFiP - INRIA                                      *)
(*                                                                            *)
(* Ce programme est distribué sous la licence CeCILL-C: vous pouvez le        *)
(* redistribuer et/ou le modifier sous les contraintes de celle-ci.           *)
(*                                                                            *)
(* L'accessibilité au code source et les droits de copie, de modification et  *)
(* de redistribution qui découlent de ce contrat ont pour contrepartie de     *)
(* n'offrir aux utilisateurs qu'une garantie limitée et de ne faire peser sur *)
(* l'auteur du logiciel, le titulaire des droits patrimoniaux et les          *)
(* concédants successifs qu'une responsabilité restreinte.                    *)
(*                                                                            *)
(******************************************************************************)

type atom = int

type replacement = { canon : atom; to_replace : atom }

type def_expr =
  | DEand of def_expr list
  | DEor of def_expr list
  | DEnot of def_expr
  | DEatom of atom

let fresh_atom =
  let i = ref 0 in
  fun () ->
    let res = !i in
    incr i;
    res

let true_ = DEand []

let false_ = DEor []

let rec not_ = function
  | DEnot e -> e
  | DEand l -> DEor (List.map not_ l)
  | DEor l -> DEand (List.map not_ l)
  | f -> DEnot f

let or_ e1 e2 =
  match (e1, e2) with
  | DEand [], _ | _, DEand [] -> DEand []
  | DEor [], e | e, DEor [] -> e
  | DEatom v1, DEatom v2 when v1 = v2 -> e1
  | e, DEnot ne when compare e ne = 0 -> DEand []
  | DEnot ne, e when compare e ne = 0 -> DEand []
  | DEor l1, DEor l2 -> DEor (l1 @ l2)
  | _, DEor l -> DEor (e1 :: l)
  | DEor l, _ -> DEor (l @ [ e2 ])
  | _, _ -> DEor [ e1; e2 ]

let and_ e1 e2 =
  match (e1, e2) with
  | DEor [], _ | _, DEor [] -> DEor []
  | DEand [], e | e, DEand [] -> e
  | e, DEnot ne when compare e ne = 0 -> DEor []
  | DEnot ne, e when compare e ne = 0 -> DEor []
  | DEatom v1, DEatom v2 when v1 = v2 -> e1
  | DEand l1, DEand l2 -> DEand (l1 @ l2)
  | _, DEand l -> DEand (e1 :: l)
  | DEand l, _ -> DEand (l @ [ e2 ])
  | _, _ -> DEand [ e1; e2 ]

let ands l =
  match l with [] -> DEand [] | hd :: tl -> List.fold_left and_ hd tl

let ors l = match l with [] -> DEor [] | hd :: tl -> List.fold_left or_ hd tl

let compare_atom = Int.compare

module AtomMap = Map.Make (Int)

module type S = sig
  type expr

  type t

  val defalse : t

  val detrue : t

  val deand : t list -> t

  val deor : t list -> t

  val denot : t -> t

  val devar : expr -> t

  val deite : t -> t -> t -> t

  val get_expr : t -> def_expr

  val get_assoc : t -> expr AtomMap.t
end

module Make (OrderedExprs : sig
  type t

  val compare : t -> t -> int
end) : S with type expr = OrderedExprs.t = struct
  type expr = OrderedExprs.t

  module ExprMap = Map.Make (OrderedExprs)

  type t = { expr : def_expr; map : atom ExprMap.t }
  (** An definition expression that can be translated back into its original
      expression type through the map. *)

  let defalse = { expr = DEor []; map = ExprMap.empty }

  let detrue = { expr = DEand []; map = ExprMap.empty }

  let rec compare e e' =
    match (e, e') with
    | DEand l, DEand l' | DEor l, DEor l' -> List.compare compare l l'
    | DEnot e, DEnot e' -> compare e e'
    | DEatom s, DEatom s' -> compare_atom s s'
    | DEand _, _ -> 1
    | _, DEand _ -> -1
    | DEor _, _ -> 1
    | _, DEor _ -> -1
    | DEnot _, _ -> 1
    | _, DEnot _ -> -1

  let uniq_list l =
    List.sort_uniq (fun { expr; _ } { expr = e'; _ } -> compare expr e') l

  (** When two expressions are created independently, their atom identifier may
      be different. This function detects when an expression has two different
      atoms and aggregates replacements to perform on the final expression. *)
  let merge_maps ~replacements m m' =
    let replacements = ref replacements in
    let map =
      ExprMap.merge
        (fun _e v v' ->
          match (v, v') with
          | Some e, None | None, Some e -> Some e
          | None, None -> None
          | Some v, Some v' ->
              if compare_atom v v' <> 0 then
                replacements := { canon = v; to_replace = v' } :: !replacements;
              Some v)
        m m'
    in
    (map, !replacements)

  (** Applies a replacement on a formula. *)
  let apply_replacement_on_expr f { canon; to_replace } =
    let rec loop = function
      | DEatom v when v = to_replace -> DEatom canon
      | DEatom _ as v -> v
      | DEnot e -> DEnot (loop e)
      | DEor l -> DEor (List.map loop l)
      | DEand l -> DEand (List.map loop l)
    in
    loop f

  (** Returns the map associated to a list of expression that will be used in a
      same formula, as well as the list of expressions updated to be consistent
      with the said map. *)
  let merge_exprs l =
    let l' = uniq_list l in
    let map, replacements =
      List.fold_left
        (fun (acc, replacements) { map; _ } -> merge_maps ~replacements map acc)
        (ExprMap.empty, []) l'
    in
    let exprs =
      List.map
        (fun l -> List.fold_left apply_replacement_on_expr l.expr replacements)
        l'
    in
    (map, exprs)

  let deand (l : t list) : t =
    let map, exprs = merge_exprs l in
    { map; expr = ands exprs }

  let deor l =
    let map, exprs = merge_exprs l in
    { map; expr = ors exprs }

  let denot e = { e with expr = not_ e.expr }

  let devar v =
    let s = fresh_atom () in
    { expr = DEatom s; map = ExprMap.singleton v s }

  let deite c t e = deor [ deand [ c; t ]; deand [ denot c; e ] ]

  let get_expr e = e.expr

  let get_assoc t =
    ExprMap.fold (fun k b acc -> AtomMap.add b k acc) t.map AtomMap.empty
end

module Shorten_def = struct
  (* From a def_expr list, returns:
     - the list of def_expr with no atom ('var' and  'not vars');
     - the map of atoms with their prefix ([true] for 'var', [false] for 'not var'). *)
  let split_forms (l : def_expr list) : def_expr list * (atom * bool) list =
    List.fold_left
      (fun (l', map) -> function
        | DEatom v -> (l', (v, true) :: map)
        | DEnot (DEatom v) -> (l', (v, false) :: map)
        | f -> (f :: l', map))
      ([], []) l

  (* From an (atom => bool) map, replaces atoms in a formula
     if they belong to the map by their truth value. If negate is
     set to true, reverses their truth value. *)
  let apply_known_on_atoms ~negate ~known f =
    let rec loop f =
      match f with
      | DEatom v -> begin
          match List.assoc v known <> negate with
          | true -> true_
          | false -> false_
          | exception Not_found -> f
        end
      | DEnot f -> not_ @@ loop f
      | DEor l -> ors (List.map loop l)
      | DEand l -> ands (List.map loop l)
    in
    loop f

  (* Reverses the behavior of the split_forms returned map. *)
  let knowns_to_form m =
    List.fold_left
      (fun acc (i, b) -> if b then DEatom i :: acc else not_ (DEatom i) :: acc)
      [] m

  (* Applies simple boolean simplifications. For any atom [v] and formulas [f]
     and [g] :
     - if [f] = [v] /\ [g], replaces occurences of [v] by [true] in [g];
     - if [f] = [v] \/ [g], replaces occurences of [v] by [false] in [g].
     
     This simplification is done recursively on formulas. *)
  let apply f =
    let rec loop f =
      match f with
      | DEatom _ -> f
      | DEnot f -> not_ (loop f)
      | DEor l ->
          (* Separates non-atoms (l) from atoms (known) *)
          let l, known = split_forms l in
          (* Apply known atoms on non-atoms formula. We negate the atoms:
             for a formula [f = atom \/ f'], we can assume occurences of [atom]
             in [f'] are false (for if they were true, [f] would be true
             anyway). *)
          let l = List.map (apply_known_on_atoms ~negate:true ~known) l in
          (* Recursively applying the whole simplification on non-atoms *)
          let l = List.map loop l in
          (* Re-building the formula list *)
          let l = knowns_to_form known @ l in
          ors l
      | DEand l ->
          (* Same procedure than DEor, except we do not negate the atoms. *)
          let l, known = split_forms l in
          let l = List.map (apply_known_on_atoms ~negate:false ~known) l in
          let l = List.map loop l in
          let l = knowns_to_form known @ l in
          ands l
    in
    loop f
end
