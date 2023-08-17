module type T = sig
  include Set.S

  val from_list : elt list -> t

  val from_marked_list : elt Pos.marked list -> t

  val pp :
    ?sep:string ->
    ?pp_elt:(Format.formatter -> elt -> unit) ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make =
functor
  (Ord : Set.OrderedType)
  ->
  struct
    include Set.Make (Ord)

    let from_list (l : elt list) : t =
      let fold set elt = add elt set in
      List.fold_left fold empty l

    let from_marked_list (l : elt Pos.marked list) : t =
      let fold set elt = add (Pos.unmark elt) set in
      List.fold_left fold empty l

    let pp_nil (_ : Format.formatter) (_ : elt) = ()

    let pp ?(sep = " ") ?(pp_elt = pp_nil) (_ : unit) (fmt : Format.formatter)
        (set : t) : unit =
      let pp_content fmt set =
        let foldSet elt first =
          let _ =
            if first then Format.fprintf fmt "%a" pp_elt elt
            else Format.fprintf fmt "%s%a" sep pp_elt elt
          in
          false
        in
        ignore (fold foldSet set true)
      in
      Format.fprintf fmt "{ %a }" pp_content set
  end
