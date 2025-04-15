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

let concat_int str f i =
  let is = Pp.spr "%d" (abs i) in
  let lis = String.length is in
  let lf = String.length f in
  if lis >= lf then str ^ is
  else
    let f' = String.sub f 0 (lf - lis) in
    str ^ f' ^ is

(* let sanitize_str (s, p) =
   String.map
     (fun c ->
       if c >= Char.chr 128 then
         let () =
           Cli.warning_print "Replaced char code %d by space %a" (Char.code c)
             Pos.format_position p
         in
         ' '
       else c)
     s *)

let sanitize_c_str s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | '\b' -> Buffer.add_string buf "\\b"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\007' -> Buffer.add_string buf "\\a"
    | '\027' -> Buffer.add_string buf "\\e"
    | '\012' -> Buffer.add_string buf "\\f"
    | '\011' -> Buffer.add_string buf "\\v"
    | ('\\' | '\'' | '"' | '?') as c -> Buffer.add_string buf (Pp.spr "\\%c" c)
    | c when c <= '\031' || '\127' <= c ->
        let code_str = Pp.spr "\\%03o" (Char.code c) in
        Buffer.add_string buf code_str
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

let compare_default = String.compare

let ascii_to_ebcdic =
  [|
    0;   1;   2;   3;   55;  45;  46;  47;  22;  5;   37;  11;  12;  13;  14;  15;
    16;  17;  18;  19;  60;  61;  50;  38;  24;  25;  63;  39;  28;  29;  30;  31;
    64;  79;  127; 123; 91;  108; 80;  125; 77;  93;  92;  78;  107; 96;  75;  97;
    240; 241; 242; 243; 244; 245; 246; 247; 248; 249; 122; 94;  76;  126; 110; 111;
    124; 193; 194; 195; 196; 197; 198; 199; 200; 201; 209; 210; 211; 212; 213; 214;
    215; 216; 217; 226; 227; 228; 229; 230; 231; 232; 233; 74;  224; 90;  95;  109;
    121; 129; 130; 131; 132; 133; 134; 135; 136; 137; 145; 146; 147; 148; 149; 150;
    151; 152; 153; 162; 163; 164; 165; 166; 167; 168; 169; 192; 106; 208; 161;
  |][@@ocamlformat "disable"]

let compare_ebcdic str1 str2 =
  let rec ebcdic_compare_aux i =
    if String.length str1 <= i || String.length str2 <= i then
      Stdlib.compare (String.length str1) (String.length str2)
    else
      let r =
        Stdlib.compare
          ascii_to_ebcdic.(Char.code str1.[i])
          ascii_to_ebcdic.(Char.code str2.[i])
      in
      if r <> 0 then r else ebcdic_compare_aux (i + 1)
  in
  ebcdic_compare_aux 0

let starts_with ~prefix s =
  let lp = String.length prefix in
  let ls = String.length s in
  let rec aux i = i = lp || (prefix.[i] = s.[i] && aux (i + 1)) in
  lp <= ls && aux 0
