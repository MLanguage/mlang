let pp_cycle pp ppf cycle =
  Format.pp_print_list
    ~pp_sep:(fun fmt _ -> Format.fprintf fmt " -> ")
    pp ppf cycle

let selected_lang ~default =
  match String.lowercase_ascii @@ Sys.getenv "LANG" with
  | "f" | "fr" | "francais" | "french" -> `Francais
  | "a" | "an" | "anglais" | "e" | "en" | "english" -> `English
  | _ | (exception Not_found) -> default
