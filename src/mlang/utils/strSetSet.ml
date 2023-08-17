include SetSetExt.Make (StrSet)

module type T = SetSetExt.T with type base_elt = string and type elt = StrSet.t

let pp ?(sep1 = ", ") ?(sep2 = " ") ?(pp_elt = Format.pp_print_string)
    (_ : unit) (fmt : Format.formatter) (setSet : t) : unit =
  pp ~sep1 ~sep2 ~pp_elt () fmt setSet
