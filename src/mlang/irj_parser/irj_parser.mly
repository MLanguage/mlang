(* Copyright Inria, contributors: Raphaël Monat <raphael.monat@lip6.fr> (2019)
   Mathieu Durero <mathieu.durero@dgfip.finances.gouv.fr> (2023)

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
%}

%token<string> SYMBOL NAME INTEGER FLOAT
/* Possibly use stronger constraints than just string on rappel's fields
some are characters, some are 0/1, etc. */

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

%token EOF

%type<irj_file> irj_file

%start irj_file

%%

irj_file:
| NOM nom = name
  fip?
  prim = primitif
  rapp = rappels
  ENDSHARP { { nom; prim; rapp } }
| EOF { assert false }

primitif:
  ENTREESPRIM
  entrees = list(variable_and_value)
  CONTROLESPRIM
  controles_attendus = list(error_code)
  RESULTATSPRIM
  resultats_attendus = list(variable_and_value) 
  {  { entrees; controles_attendus; resultats_attendus } }

rappels:
/* The two constructions match respectively corrective test files and primary test files */
| ENTREESRAPP
  entrees_rappels = list(rappel)
  CONTROLESRAPP
  controles_attendus = list(error_code)
  RESULTATSRAPP
  resultats_attendus = list(variable_and_value) 
  { Some { entrees_rappels; controles_attendus; resultats_attendus} }
| ENTREESCORR CONTROLESCORR RESULTATSCORR DATES? AVISIR? AVISCSG? { None }

name:
| n = NAME { n }
| n = SYMBOL { n }

fip:
  FIP SLASH option(SYMBOL) { }

variable_and_value:
| var = SYMBOL SLASH value = INTEGER  { (var, I (int_of_string value), Parse_utils.mk_position $sloc) }
| var = SYMBOL SLASH value = FLOAT  { (var, F (float_of_string value), Parse_utils.mk_position $sloc) }

error_code:
  error = SYMBOL { (error, Parse_utils.mk_position $sloc) }

rappel:
  event_nb = INTEGER SLASH
  rappel_nb = INTEGER SLASH
  variable_change = variable_and_value SLASH
  direction = SYMBOL SLASH
  penalty_code = INTEGER SLASH
  base_tolerance_legale = INTEGER SLASH
  month_year = INTEGER SLASH
  decl_2042_rect = INTEGER
  { (event_nb, 
     rappel_nb,
     variable_change,
     direction,
     penalty_code,
     base_tolerance_legale,
     month_year,
     decl_2042_rect) }
