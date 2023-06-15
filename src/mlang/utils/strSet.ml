include Set.Make (String)

let from_list (l : string list) : t =
  let fold set str = add str set in
  List.fold_left fold empty l

let from_marked_list (l : string Pos.marked list) : t =
  let fold set str = add (Pos.unmark str) set in
  List.fold_left fold empty l

let pp (sep : string) (fmt : Format.formatter) (set : t) : unit =
  let foldSet str first =
    let _ =
      if first then Format.fprintf fmt "%s" str
      else Format.fprintf fmt "%s%s" sep str
    in
    false
  in
  ignore (fold foldSet set true)
