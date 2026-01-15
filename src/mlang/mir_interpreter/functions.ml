(* TODO: move functions here *)
open M_ir
open Types

module Make
    (N : Mir_number.NumberInterface)
    (R : Mir_roundops.RoundOpsInterface with type t = N.t) =
struct
  let false_value () = Number (N.zero ())

  let true_value () = Number (N.one ())

  let arr = function
    | Number x -> Number (R.roundf x)
    | Undefined -> Undefined (*nope:Float 0.*)

  let inf = function
    | Number x -> Number (R.truncatef x)
    | Undefined -> Undefined

  let present = function Undefined -> false_value () | _ -> true_value ()

  let supzero = function
    | Undefined -> Undefined
    | Number f as n -> if N.compare Com.Lte f (N.zero ()) then Undefined else n

  let abs = function Undefined -> Undefined | Number f -> Number (N.abs f)

  let min i j =
    match (i, j) with
    | Undefined, Undefined -> Undefined
    | Undefined, Number f | Number f, Undefined -> Number (N.min (N.zero ()) f)
    | Number fl, Number fr -> Number (N.min fl fr)

  let max i j =
    match (i, j) with
    | Undefined, Undefined -> Undefined
    | Undefined, Number f | Number f, Undefined -> Number (N.max (N.zero ()) f)
    | Number fl, Number fr -> Number (N.max fl fr)

  let multimax (i : N.t value)
      (j : [ `Table of N.t value list | `Var of N.t value ]) : N.t value =
    match (i, j) with
    | Undefined, _ -> Undefined
    | Number n, _ when N.is_zero n -> Undefined
    | Number f, `Table l ->
        let nb = Int64.to_int @@ N.to_int @@ R.roundf f in
        let rec loop res cpt = function
          | [] -> res
          | _ when cpt >= nb -> res
          | Undefined :: tl -> loop res (cpt + 1) tl
          | (Number v as hd) :: tl ->
              let res =
                match res with
                | Undefined -> hd
                | Number nr -> if N.(nr <. v) then hd else res
              in
              loop res (cpt + 1) tl
        in
        loop Undefined 0 l
    | Number _, `Var v -> v

  let nb_events (ctx : 'a ctx) =
    let card = Array.length (List.hd ctx.ctx_events) in
    Number (N.of_int @@ Int64.of_int @@ card)
end
