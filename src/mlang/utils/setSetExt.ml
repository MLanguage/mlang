module type T = sig
  type base_elt

  include SetExt.T

  val from_list_list : base_elt list list -> t

  val from_marked_list_list : base_elt Pos.marked list Pos.marked list -> t

  val pp :
    ?sep1:string ->
    ?sep2:string ->
    ?pp_elt:(Format.formatter -> base_elt -> unit) ->
    unit ->
    Format.formatter ->
    t ->
    unit
end

module Make =
functor
  (SetElt : SetExt.T)
  ->
  struct
    type base_elt = SetElt.elt

    include SetExt.Make (SetElt)

    let from_list_list (ll : base_elt list list) : t =
      let fold setSet l = add (SetElt.from_list l) setSet in
      List.fold_left fold empty ll

    let from_marked_list_list (ll : base_elt Pos.marked list Pos.marked list) :
        t =
      let fold setSet l = add (SetElt.from_marked_list (Pos.unmark l)) setSet in
      List.fold_left fold empty ll

    let pp_nil (_ : Format.formatter) (_ : base_elt) = ()

    let pp ?(sep1 = ", ") ?(sep2 = " ") ?(pp_elt = pp_nil) (_ : unit)
        (fmt : Format.formatter) (setSet : t) : unit =
      let foldSetSet set first =
        let _ =
          if first then
            Format.fprintf fmt "%a" (SetElt.pp ~sep:sep2 ~pp_elt ()) set
          else
            Format.fprintf fmt "%s%a" sep1 (SetElt.pp ~sep:sep2 ~pp_elt ()) set
        in
        false
      in
      ignore (fold foldSetSet setSet true)
  end
