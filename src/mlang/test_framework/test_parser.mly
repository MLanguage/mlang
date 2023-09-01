(*
Copyright Inria, contributors:
  Raphaël Monat <raphael.monat@lip6.fr> (2019)

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

%{ open Test_ast
   open Parse_utils

let error (sp, ep) msg =
  Errors.raise_spanned_error ("Parse error : " ^ msg)
    (Parse_utils.mk_position (sp, ep))

%}

%token<string> SYMBOL NAME
%token<int> INTEGER
%token<float> FLOAT

%token SLASH
%token NOM FIP
%token ENTREESP CONTROLESP RESULTATSP
%token ENTREESC CONTROLESC RESULTATSC
%token ENTREESR CONTROLESR RESULTATSR
%token DATES AVISIR AVISCSG
%token ENDSHARP
%token NL

%token EOF

%type<Test_ast.test_file> test_file

%start test_file

%%



test_file:
| NOM NL
  nom = name*
  fip?
  prim = primitif
  corr = correctif?
  rapp = rappels?
  DATES?
  AVISIR?
  AVISCSG?
  endsharp NL? (* newline at end-of-file is optional *)
  EOF {
    let nom =
      match nom with
      | [n] -> n
      | [] -> error $loc(nom) "Missing name in section #NOM"
      | _ -> error $loc(nom) "Extra line(s) in section #NOM"
    in
    let ep, cp, rp = prim in
    { nom; ep; cp; rp; corr; rapp }
  }
| EOF { error $loc "Empty test file" }

name:
| n = name_or_symbol NL { n }

fip:
| FIP SLASH SYMBOL? NL { } (* it is actually allowed to leave it blank *)

primitif:
| ENTREESP NL
  ep = input*
  CONTROLESP NL
  cp = discord*
  RESULTATSP NL
  rp = input* { (ep, cp, rp) }

correctif:
| ENTREESC NL
  ec = input*
  CONTROLESC NL
  cc = discord*
  RESULTATSC NL
  rc = input* { (ec, cc, rc) }

rappels:
| ENTREESR NL
  ec = rappel*
  CONTROLESR NL
  cc = discord*
  RESULTATSR NL
  rc = input* { (ec, cc, rc) }

discord:
| d = SYMBOL NL { (d, mk_position $sloc) }

input:
| var = SYMBOL SLASH value = value NL { (var, value, mk_position $sloc) }
| SYMBOL error                        { error $loc "Missing slash" }

rappel:
| num_evt = integer SLASH
  num_rap = integer SLASH
  variable = SYMBOL SLASH
  value = integer SLASH
  sens = SYMBOL SLASH
  penalite = INTEGER? SLASH
  base_tl = INTEGER? SLASH
  date_inr = INTEGER SLASH
  ind20 = INTEGER? NL {
    if sens <> "R" && sens <> "C" && sens <> "M" && sens <> "P" then
      error $loc(sens) ("Unknown value for 'sens' : " ^ sens);
    let p = match penalite with Some p -> p | _ -> 0 in
    if p < 0 || p > 99 then
      error $loc(sens) ("Invalid value for 'penalite' : " ^ (string_of_int p));
    { num_evt; num_rap; variable; value; sens; penalite;
      base_tl; date_inr; ind20; pos = mk_position $sloc }
  }

integer:
| i = INTEGER { i }
| error       { error $loc "Missing integer" }

value:
| i = INTEGER { Int (i) }
| f = FLOAT   { Float (f) }
| error       { error $loc "Missing value" }

name_or_symbol:
| n = NAME    { n }
| n = SYMBOL  { n }

endsharp:
| ENDSHARP    { () }
| error       { error $loc "Missing ## at end of file" }
