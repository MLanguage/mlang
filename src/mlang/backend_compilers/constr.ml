module VID = Dgfip_varid

type dflag = Def | Val | VarInfo | VarSpace

type local_var =
  | Anon (* inlined sub-expression, not intended for reuse *)
  | Refered of int
(* declared local variable, either M local or locally bound in the constructors
   below *)

(** Contructors used for building C instructions. *)
type t =
  | True
  | False
  | Lit of float
  | M of Com.var_space * Com.Var.t * dflag
  | Local of local_var
  | And of t * t
  | Or of t * t
  | Not of t
  | Minus of t
  | Plus of t * t
  | Sub of t * t
  | Mult of t * t
  | Div of t * t
  | Modulo of t * t
  | Comp of string * t * t
  | Fun of string * t list
  | Varinfo of Dgfip_varid.varinfo
  | Varinfo_tab of Dgfip_varid.varinfo * t * t
  | Varinfo_field of t * t * string
  | Varspace_current of Com.var_space
  | Varspace_of of Com.var_space * Dgfip_varid.varinfo
  | Typ of Com.value_typ
  | Instr of string
  | Direct of t
  | Ite of t * t * t
  | It0 of t * t
  | Let_local of local_var * t * t

let irdata = Direct (Instr "irdata")

let dflag_id = function Def -> 0 | Val -> 1 | VarInfo -> 2 | VarSpace -> 3

let typ_id = function
  | Com.Boolean -> 0
  | DateYear -> 1
  | DateDayMonthYear -> 2
  | DateMonth -> 3
  | Integer -> 4
  | Real -> 5

let compare_dflag d d' = Int.compare (dflag_id d) (dflag_id d')

let compare_typ t t' = Int.compare (typ_id t) (typ_id t')

let compare_local_var lv lv' =
  match (lv, lv') with
  | Anon, Anon -> 0
  | Refered i, Refered i' -> Int.compare i i'
  | Anon, _ -> 1
  | _, Anon -> -1

(** Operator to aggregate comparisons *)
let ( >< ) i j = if i <> 0 then i else j

let compare_varspace =
  Option.compare (fun (Pos.Mark (i, _), _) (Pos.Mark (j, _), _) ->
      String.compare (Com.get_var_name i) (Com.get_var_name j))

let rec compare (c1 : t) (c2 : t) : int =
  match (c1, c2) with
  | True, True | False, False -> 0
  | Lit f, Lit f' -> Float.compare f f'
  | M (vs, v, d), M (vs', v', d') ->
      Int.compare v.id v'.id >< compare_dflag d d' >< compare_varspace vs vs'
  (* | Local lv, Local lv' -> compare_local_var lv lv' *)
  | And (e1, e2), And (e1', e2')
  | Or (e1, e2), Or (e1', e2')
  | Plus (e1, e2), Plus (e1', e2')
  | Sub (e1, e2), Sub (e1', e2')
  | Mult (e1, e2), Mult (e1', e2')
  | Div (e1, e2), Div (e1', e2')
  | Modulo (e1, e2), Modulo (e1', e2') ->
      compare e1 e1' >< compare e2 e2'
  | Not e, Not e' | Minus e, Minus e' -> compare e e'
  | Comp (s, e1, e2), Comp (s', e1', e2') ->
      String.compare s s' >< compare e1 e1' >< compare e2 e2'
  | Fun (s, l), Fun (s', l') -> String.compare s s' >< List.compare compare l l'
  | Varinfo v, Varinfo v' -> Int.compare v.id v'.id
  | Varinfo_tab (vi, d, v), Varinfo_tab (vi', d', v') ->
      Int.compare vi.id vi'.id >< compare d d' >< compare v v'
  | Varinfo_field (d, v, s), Varinfo_field (d', v', s') ->
      compare d d' >< compare v v' >< String.compare s s'
  | Varspace_current vs, Varspace_current vs' -> compare_varspace vs vs'
  | Varspace_of (vs, vi), Varspace_of (vs', vi') ->
      compare_varspace vs vs' >< Int.compare vi.id vi'.id
  | Typ t, Typ t' -> compare_typ t t'
  | Instr s, Instr s' -> String.compare s s'
  | Direct t, Direct t' -> compare t t'
  | Ite (c, t, e), Ite (c', t', e') ->
      compare c c' >< compare t t' >< compare e e'
  | It0 (c, t), It0 (c', t') -> compare c c' >< compare t t'
  | Let_local (lv, e1, e2), Let_local (lv', e1', e2') ->
      compare_local_var lv lv' >< compare e1 e1' >< compare e2 e2'
  | True, _ -> 1
  | _, True -> -1
  | False, _ -> 1
  | _, False -> -1
  | Lit _, _ -> 1
  | _, Lit _ -> -1
  | M _, _ -> 1
  | _, M _ -> -1
  | Local _, _ -> 1
  | _, Local _ -> -1
  | And _, _ -> 1
  | _, And _ -> -1
  | Or _, _ -> 1
  | _, Or _ -> -1
  | Not _, _ -> 1
  | _, Not _ -> -1
  | Minus _, _ -> 1
  | _, Minus _ -> -1
  | Plus _, _ -> 1
  | _, Plus _ -> -1
  | Sub _, _ -> 1
  | _, Sub _ -> -1
  | Mult _, _ -> 1
  | _, Mult _ -> -1
  | Div _, _ -> 1
  | _, Div _ -> -1
  | Modulo _, _ -> 1
  | _, Modulo _ -> -1
  | Comp _, _ -> 1
  | _, Comp _ -> -1
  | Fun _, _ -> 1
  | _, Fun _ -> -1
  | Varinfo _, _ -> 1
  | _, Varinfo _ -> -1
  | Varinfo_tab _, _ -> 1
  | _, Varinfo_tab _ -> -1
  | Varinfo_field _, _ -> 1
  | _, Varinfo_field _ -> -1
  | Varspace_current _, _ -> 1
  | _, Varspace_current _ -> -1
  | Varspace_of _, _ -> 1
  | _, Varspace_of _ -> -1
  | Typ _, _ -> 1
  | _, Typ _ -> -1
  | Instr _, _ -> 1
  | _, Instr _ -> -1
  | Direct _, _ -> 1
  | _, Direct _ -> -1
  | Ite _, _ -> 1
  | _, Ite _ -> -1
  | It0 _, _ -> 1
  | _, It0 _ -> -1
(* | Let_local _, _ -> 1 | _, Let_local _ -> -1  *)

(** smart constructors *)

let anon = Anon

let locals_from_m =
  let counter = ref 0 in
  let fresh_id () =
    let v = !counter in
    counter := !counter + 1;
    v
  in
  fun () ->
    let lvar_id = fresh_id () in
    (Refered (-(2 * lvar_id)), Refered (-((2 * lvar_id) + 1)))

let new_local : unit -> local_var =
  let c = ref 0 in
  fun () ->
    let i = !c in
    incr c;
    Refered i
