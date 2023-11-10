(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   Mathieu Durero <mathieu.durero@dgfip.finances.gouv.fr> (2023)
   David Declerck (2023)

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

%{ open Irj_ast
   
  let error (sp, ep) msg =
  Errors.raise_spanned_error ("Parse error : " ^ msg)
    (Parse_utils.mk_position (sp, ep))

%}

%token<string> SYMBOL NAME
%token<int> INTEGER
%token<float> FLOAT

%token SLASH
/* Used as field separator */
%token NOM FIP
/* Identifiers */
%token ENTREESPRIM CONTROLESPRIM RESULTATSPRIM
/* Primary computation data blocks */
%token ENTREESRAPP CONTROLESRAPP RESULTATSRAPP
/* Corrective computation data blocks */
%token ENTREESCORR CONTROLESCORR RESULTATSCORR
/* Old form of corrective data blocks, always present and empty in primary computation test files */
%token DATES AVISIR AVISCSG
/* Old empty data blocks rarely found in primary files from 2019 and older */
%token ENDSHARP
/* Mark the end of a record */

%token NL
/* New line */
%token EOF

%type<irj_file> irj_file

%start irj_file

%%

irj_file:
| NOM NL
  nom = list(name)
  fip?
  prim = primitif
  rapp = rappels
  endsharp NL
  EOF {     
    let nom =
      match nom with
      | [n] -> n
      | [] -> error $loc(nom) "Missing name in section #NOM"
      | _ -> error $loc(nom) "Extra line(s) in section #NOM"
    in
    { nom; prim; rapp } }
| EOF { error $loc "Empty test file" }

/* What's the point of this alternative?*/
name:
| n = NAME NL { n }
| n = SYMBOL NL { n }

fip:
  FIP SLASH SYMBOL? NL { } (* it is actually allowed to leave it blank *)

primitif:
  ENTREESPRIM NL
  entrees = list(variable_and_value)
  CONTROLESPRIM NL
  controles_attendus = list(calc_error)
  RESULTATSPRIM NL
  resultats_attendus = list(variable_and_value) 
  {  { entrees; controles_attendus; resultats_attendus } }

rappels:
/* The two constructions match respectively corrective test files and primary test files */
| ENTREESRAPP NL
  entrees_rappels = list(rappel)
  CONTROLESRAPP NL
  controles_attendus = list(calc_error)
  RESULTATSRAPP NL
  resultats_attendus = list(variable_and_value) 
  { Some { entrees_rappels; controles_attendus; resultats_attendus} }
| ENTREESCORR NL CONTROLESCORR NL RESULTATSCORR NL DATES? AVISIR? AVISCSG? { None }

variable_and_value:
| var = SYMBOL SLASH value = value NL { (var, value, Parse_utils.mk_position $sloc) }
| SYMBOL error { error $loc "Missing slash in pair variable/value" }

calc_error:
  error = SYMBOL NL { (error, Parse_utils.mk_position $sloc) }

rappel:
  event_nb = integer SLASH
  rappel_nb = integer SLASH
  variable_code = SYMBOL SLASH
  change_value = integer SLASH
  direction = SYMBOL SLASH
  penalty_code = INTEGER? SLASH
  base_tolerance_legale = INTEGER? SLASH
  month_year = integer SLASH
  decl_2042_rect = INTEGER? NL
  {
    if direction <> "R" && direction <> "C" && direction <> "M" && direction <> "P" then
      error $loc(direction) ("Unknown value for 'direction' (type of the 'rappel', should be R, C, M or P) : " ^ direction);
    let p = match penalty_code with Some p -> p | _ -> 0 in
    if p < 0 || p > 99 then
      error $loc(direction) ("Invalid value for 'penalty_code' (out of range 0-99) : " ^ (string_of_int p));
    {event_nb; 
     rappel_nb;
     variable_code;
     change_value;
     direction;
     penalty_code;
     base_tolerance_legale;
     month_year;
     decl_2042_rect; 
     pos = Parse_utils.mk_position $sloc }
  }

integer:
| i = INTEGER { i }
| error       { error $loc "Missing integer" }

value:
| i = INTEGER { I (i) }
| f = FLOAT   { F (f) }
| error       { error $loc "Missing numerical value" }

endsharp:
| ENDSHARP    { () }
| error       { error $loc "Missing ## at end of file" }
