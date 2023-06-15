include Set.Make (StrSet)

let from_list_list (ll : string list list) : t =
  let fold setSet l = add (StrSet.from_list l) setSet in
  List.fold_left fold empty ll

let from_marked_list_list (ll : string Pos.marked list Pos.marked list) : t =
  let fold setSet l = add (StrSet.from_marked_list (Pos.unmark l)) setSet in
  List.fold_left fold empty ll

let pp (sep1 : string) (sep2 : string) (fmt : Format.formatter) (setSet : t) :
    unit =
  let foldSetSet set first =
    let _ =
      if first then Format.fprintf fmt "%a" (StrSet.pp sep2) set
      else Format.fprintf fmt "%s%a" sep1 (StrSet.pp sep2) set
    in
    false
  in
  ignore (fold foldSetSet setSet true)
