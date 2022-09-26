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

%{ open Types_module 
%}

%token<string> SYMBOL NAME INTEGER FLOAT

%token SLASH
%token NOM FIP
%token ENTREESP CONTROLESP RESULTATSP
%token ENTREESC CONTROLESC RESULTATSC
%token DATES AVISIR AVISCSG
%token ENDSHARP

%token EOF

%type<Types_module.test_file> test_file

%start test_file

%%



test_file:
| NOM nom = name
  fip?
  ENTREESP
  ep = list(variable_and_value)
  CONTROLESP
  cp = list(variable_and_value)
  RESULTATSP
  rp = list(variable_and_value)
  corr = correctif?
  DATES?
  AVISIR?
  AVISCSG?
  ENDSHARP { { nom; ep; cp; rp; corr } }
| EOF { assert false }

correctif:
  ENTREESC
  ec = list(variable_and_value)
  CONTROLESC
  cc = list(variable_and_value)
  RESULTATSC
  rc = list(variable_and_value) { (ec, cc, rc) }


name:
| n = NAME { n }
| n = SYMBOL { n }

fip:
  FIP SLASH option(SYMBOL) { }

variable_and_value:
| var = SYMBOL SLASH value = INTEGER  { (var, I (int_of_string value), mk_position $sloc) }
| var = SYMBOL SLASH value = FLOAT  { (var, F (float_of_string value), mk_position $sloc) }
