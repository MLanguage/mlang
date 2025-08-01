module type T = sig
  include Map.S

  val card : 'a t -> int

  val one : key -> 'a -> 'a t

  val from_assoc_list : (key * 'a) list -> 'a t

  val union_fst : 'a t -> 'a t -> 'a t

  val union_snd : 'a t -> 'a t -> 'a t

  val pp :
    ?sep:string ->
    ?pp_key:(Pp.t -> key -> unit) ->
    ?assoc:string ->
    (Pp.t -> 'a -> unit) ->
    Pp.t ->
    'a t ->
    unit

  val pp_keys :
    ?sep:string -> ?pp_key:(Pp.t -> key -> unit) -> unit -> Pp.t -> 'a t -> unit
end

module Make =
functor
  (Ord : Map.OrderedType)
  ->
  struct
    include Map.Make (Ord)

    let card = cardinal

    let one = singleton

    let from_assoc_list (l : (key * 'a) list) : 'a t =
      let fold map (k, v) = add k v map in
      List.fold_left fold empty l

    let union_fst map0 map1 =
      let merge_fun _ vo0 vo1 =
        match (vo0, vo1) with
        | None, None -> None
        | None, Some v | Some v, None -> Some v
        | Some v0, Some _v1 -> Some v0
      in
      merge merge_fun map0 map1

    let union_snd map0 map1 = union_fst map1 map0

    let pp ?(sep = "; ") ?(pp_key = Pp.nil) ?(assoc = " => ")
        (pp_val : Pp.t -> 'a -> unit) (fmt : Pp.t) (map : 'a t) : unit =
      let pp_content fmt map =
        let foldMap k v first =
          let _ =
            if first then Format.fprintf fmt "%a%s%a" pp_key k assoc pp_val v
            else Format.fprintf fmt "%s%a%s%a" sep pp_key k assoc pp_val v
          in
          false
        in
        ignore (fold foldMap map true)
      in
      Format.fprintf fmt "{ %a }" pp_content map

    let pp_keys ?(sep = "; ") ?(pp_key = Pp.nil) (_ : unit) (fmt : Pp.t)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc:"" Pp.nil fmt map
  end
