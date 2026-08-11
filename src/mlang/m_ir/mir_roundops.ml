(******************************************************************************)
(*                                                                            *)
(* Droit d'auteur (c) 2022 - 2026 DGFiP - INRIA                               *)
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

module type RoundOpsInterface = sig
  type t

  val truncatef : t -> t

  val roundf : t -> t
end

module type RoundOpsFunctor = functor (N : Mir_number.NumberInterface) ->
  RoundOpsInterface with type t = N.t

module DefaultRoundOps (N : Mir_number.NumberInterface) :
  RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let epsilon = !Config.comparison_error_margin

  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float epsilon)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use
     copysign here because [x < zero] is critical to have the correct behavior
     on -0 *)
  let roundf (x : N.t) =
    let e = N.of_float (0.5 +. (epsilon *. 50.)) in
    if N.(x < zero ()) then N.ceil N.(x -. e) else N.floor N.(x +. e)
end

module MultiRoundOps (N : Mir_number.NumberInterface) :
  RoundOpsInterface with type t = N.t = struct
  type t = N.t

  let epsilon = !Config.comparison_error_margin

  let truncatef (x : N.t) : N.t = N.floor N.(x +. N.of_float epsilon)

  let roundf (x : N.t) =
    let e = N.of_float (0.5 +. (epsilon *. 50.)) in
    if N.(x < zero ()) then N.ceil N.(x -. e) else N.floor N.(x +. e)
end

module MainframeRoundOps
    (L : sig
      val max_long : Int64.t ref
    end)
    (N : Mir_number.NumberInterface) : RoundOpsInterface with type t = N.t =
struct
  type t = N.t

  let epsilon = !Config.comparison_error_margin

  let floor_g (x : N.t) : N.t =
    if N.abs x <= N.of_int !L.max_long then N.floor x else x

  let ceil_g (x : N.t) : N.t =
    if N.abs x <= N.of_int !L.max_long then N.ceil x else x

  let truncatef (x : N.t) : N.t = floor_g N.(x +. N.of_float epsilon)

  (* Careful : rounding in M is done with this arbitrary behavior. We can't use
     copysign here because [x < zero] is critical to have the correct behavior
     on -0 *)
  let roundf (x : N.t) =
    let e = N.of_float (0.5 +. (epsilon *. 50.)) in
    if N.(x < zero ()) then ceil_g N.(x -. e) else floor_g N.(x +. e)
end
