(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation, either version 3 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>. *)

module E = Errors

let mk_position sloc = Pos.make_position (fst sloc).Lexing.pos_fname sloc

(** {1 Frontend variable names}*)

let parse_variable_name sloc (s : string) : string =
  if not (String.equal (String.uppercase_ascii s) s) then
    E.raise_spanned_error "invalid variable name" (mk_position sloc)
  else s

let parse_parameter sloc (s : string) : char =
  if String.length s <> 1 then
    E.raise_spanned_error "invalid parameter" (mk_position sloc)
  else
    let p = s.[0] in
    if p < 'a' || 'z' < p then
      E.raise_spanned_error "invalid parameter" (mk_position sloc)
    else p

(** Checks for duplicate generic parameters *)
let dup_exists l =
  let rec dup_consecutive = function
    | [] | [ _ ] -> false
    | c1 :: (c2 as h2) :: tl -> Char.equal c1 c2 || dup_consecutive (h2 :: tl)
  in
  let sort_on_third s1 s2 = Char.compare s1 s2 in
  dup_consecutive (List.sort sort_on_third l)

(** Parse variable with parameters, parameters have to be lowercase letters *)
let parse_variable_generic_name sloc (s : string) : Com.variable_generic_name =
  let parameters = ref [] in
  for i = String.length s - 1 downto 0 do
    let p = s.[i] in
    if
      p = '_'
      || Re.Str.string_match (Re.Str.regexp "[0-9]+") (String.make 1 p) 0
      || not (Char.equal (Char.lowercase_ascii p) p)
    then ()
    else parameters := p :: !parameters
  done;
  if dup_exists !parameters then
    E.raise_spanned_error "variable parameters should have distinct names"
      (mk_position sloc);
  { Com.parameters = !parameters; Com.base = s }

let parse_variable sloc (s : string) =
  try Com.Normal (parse_variable_name sloc s)
  with E.StructuredError _ -> (
    try Com.Generic (parse_variable_generic_name sloc s)
    with E.StructuredError _ ->
      E.raise_spanned_error "invalid variable name" (mk_position sloc))

type parse_val = ParseVar of Com.variable_name | ParseInt of int

let parse_variable_or_int sloc (s : string) : parse_val =
  try ParseInt (int_of_string s)
  with Failure _ -> (
    try ParseVar (Com.Normal (parse_variable_name sloc s))
    with E.StructuredError _ -> (
      try ParseVar (Com.Generic (parse_variable_generic_name sloc s))
      with E.StructuredError _ ->
        E.raise_spanned_error "invalid variable name" (mk_position sloc)))

let parse_table_size (s : string) : Mast.table_size =
  try Mast.LiteralSize (int_of_string s) with Failure _ -> Mast.SymbolSize s

(**{1 Literal parsing}*)

let parse_literal sloc (s : string) : Com.literal =
  try Com.Float (float_of_string s)
  with Failure _ -> E.raise_spanned_error "invalid literal" (mk_position sloc)

let parse_atom sloc (s : string) : Com.variable_name Com.atom =
  try Com.AtomLiteral (Com.Float (float_of_string s))
  with Failure _ -> Com.AtomVar (parse_variable sloc s)

let parse_func_name _ (s : string) : Mast.func_name = s

let parse_int sloc (s : string) : int =
  try int_of_string s
  with Failure _ ->
    E.raise_spanned_error "should be an integer" (mk_position sloc)

(** Parse function name *)
let parse_function_name f_name =
  let open Com in
  let map = function
    | "somme" -> SumFunc
    | "min" -> MinFunc
    | "max" -> MaxFunc
    | "abs" -> AbsFunc
    | "positif" -> GtzFunc
    | "positif_ou_nul" -> GtezFunc
    | "null" -> NullFunc
    | "arr" -> ArrFunc
    | "inf" -> InfFunc
    | "present" -> PresentFunc
    | "multimax" -> Multimax
    | "supzero" -> Supzero
    | "numero_verif" -> VerifNumber
    | "numero_compl" -> ComplNumber
    | "nb_evenements" -> NbEvents
    | fn -> Func fn
  in
  Pos.map_under_mark map f_name

(* # parse_string #
 * Takes a litteral string and produces a String.t of the corresponding chars
 *)
let parse_string (s : string) : string =
  (* we remove the quotes (first and last chars) *)
  let s = Re.Str.string_after s 1 in
  let s = Re.Str.string_before s (String.length s - 1) in
  let l = String.length s in
  let buf = Buffer.create l in
  (* We decode litteral encoded chars (i.e. "\t" "\xA0", etc.) *)
  let rec aux = function
    | i when i >= l -> Buffer.contents buf
    | i -> begin
        let c = s.[i] in
        match c with
        | '\\' -> (
            let i = i + 1 in
            if i >= l then aux i
            else
              let c = s.[i] in
              match c with
              | 'n' ->
                  Buffer.add_char buf '\n';
                  aux (i + 1)
              | 't' ->
                  Buffer.add_char buf '\t';
                  aux (i + 1)
              | '"' ->
                  Buffer.add_char buf '"';
                  aux (i + 1)
              | '\\' ->
                  Buffer.add_char buf '\\';
                  aux (i + 1)
              | 'x' | 'X' -> (
                  try
                    let to_int i =
                      if i >= l then raise Not_found
                      else
                        let c = s.[i] in
                        match c with
                        | '0' .. '9' -> int_of_char c - int_of_char '0'
                        | 'a' .. 'f' -> int_of_char c - int_of_char 'a' + 10
                        | 'A' .. 'F' -> int_of_char c - int_of_char 'A' + 10
                        | _ -> raise Not_found
                    in
                    let c1 = to_int (i + 1) in
                    let c0 = to_int (i + 2) in
                    Buffer.add_char buf (char_of_int ((c1 * 16) + c0));
                    aux (i + 3)
                  with Not_found -> aux (i + 3))
              | _ -> aux (i + 1))
        | c ->
            Buffer.add_char buf c;
            aux (i + 1)
      end
  in
  aux 0

let parse_index_format (m_s : string Pos.marked) : string Pos.marked =
  let s = Pos.unmark m_s in
  let len = String.length s in
  for i = 0 to len - 1 do
    if s.[i] <> '0' then
      Errors.raise_spanned_error "bad index format, must be 0..." (Pos.get m_s)
  done;
  m_s

let parse_if_then_etc l =
  let rec aux = function
    | [ (Some e, ilt, pos) ] -> [ (Com.IfThenElse (e, ilt, []), pos) ]
    | [ (None, ile, _pos) ] -> ile
    | (Some e, ilt, pos) :: le -> [ (Com.IfThenElse (e, ilt, aux le), pos) ]
    | _ -> assert false
  in
  match aux l with [ (i, _pos) ] -> i | _ -> assert false

let parse_when_do_etc (twl, ed) = Com.WhenDoElse (twl, ed)

type target_header =
  | Target_apps of Mast.application Pos.marked list
  | Target_input_arg of string Pos.marked list
  | Target_tmp_vars of
      (string Pos.marked * Mast.table_size Pos.marked option) list
  | Function_result of string Pos.marked

let parse_target_or_function_header name is_function header =
  let rec aux apps_opt args_opt vars_opt res_opt = function
    | (Target_apps apps', pos) :: h ->
        let apps_opt' =
          match apps_opt with
          | None -> Some (apps', pos)
          | Some (_, old_pos) ->
              Errors.raise_spanned_error
                (Format.asprintf "application list already declared %a"
                   Pos.format_position old_pos)
                pos
        in
        aux apps_opt' args_opt vars_opt res_opt h
    | (Target_input_arg vars', pos) :: h ->
        let args_opt =
          match args_opt with
          | None -> Some (vars', pos)
          | Some (_, old_pos) ->
              Errors.raise_spanned_error
                (Format.asprintf "argument list already declared %a"
                   Pos.format_position old_pos)
                pos
        in
        aux apps_opt args_opt vars_opt res_opt h
    | (Target_tmp_vars vars', pos) :: h ->
        let vars_opt' =
          match vars_opt with
          | None -> Some (vars', pos)
          | Some (_, old_pos) ->
              Errors.raise_spanned_error
                (Format.asprintf "temporary variable list already declared %a"
                   Pos.format_position old_pos)
                pos
        in
        aux apps_opt args_opt vars_opt' res_opt h
    | (Function_result res', pos) :: h ->
        if is_function then
          let res_opt' =
            match res_opt with
            | None -> Some (res', pos)
            | Some (_, old_pos) ->
                Errors.raise_spanned_error
                  (Format.asprintf "result variable already declared %a"
                     Pos.format_position old_pos)
                  pos
          in
          aux apps_opt args_opt vars_opt res_opt' h
        else aux apps_opt args_opt vars_opt res_opt h
    | [] ->
        let apps =
          match apps_opt with
          | Some (apps, _) ->
              List.fold_left
                (fun res (app, pos) ->
                  match StrMap.find_opt app res with
                  | Some (_, old_pos) ->
                      let msg =
                        Format.asprintf "application %s already declared %a" app
                          Pos.format_position old_pos
                      in
                      Errors.raise_spanned_error msg pos
                  | None -> StrMap.add app (app, pos) res)
                StrMap.empty apps
          | None ->
              let ty = if is_function then "function" else "target" in
              Errors.raise_spanned_error
                (Format.sprintf "this %s doesn't belong to an application" ty)
                (Pos.get name)
        in
        let args = match args_opt with None -> [] | Some (l, _) -> l in
        let vars =
          List.fold_left
            (fun res (vnm, vt) ->
              let vn, pos = vnm in
              match StrMap.find_opt vn res with
              | Some ((_, old_pos), _) ->
                  let msg =
                    Format.asprintf "temporary variable %s already declared %a"
                      vn Pos.format_position old_pos
                  in
                  Errors.raise_spanned_error msg pos
              | None -> StrMap.add vn (vnm, vt) res)
            StrMap.empty
            (match vars_opt with None -> [] | Some (l, _) -> l)
        in
        let res =
          match res_opt with
          | None ->
              if is_function then
                Errors.raise_spanned_error "this function doesn't have a result"
                  (Pos.get name)
              else None
          | Some (rvar, _) -> Some rvar
        in
        (apps, args, vars, res)
  in
  aux None None None None header
