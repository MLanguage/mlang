module type T = sig
  include Map.S

  val from_assoc_list : (key * 'a) list -> 'a t

  val pp :
    ?sep:string ->
    ?pp_key:(Format.formatter -> key -> unit) ->
    ?assoc:string ->
    (Format.formatter -> 'a -> unit) ->
    Format.formatter ->
    'a t ->
    unit
end

module Make =
functor
  (Ord : Map.OrderedType)
  ->
  struct
    include Map.Make (Ord)

    let from_assoc_list (l : (key * 'a) list) : 'a t =
      let fold map (k, v) = add k v map in
      List.fold_left fold empty l

    let pp_nil (_ : Format.formatter) (_ : 'b) = ()

    let pp ?(sep = "; ") ?(pp_key = pp_nil) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
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
  end
