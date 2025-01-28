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
