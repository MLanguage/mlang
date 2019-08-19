(*
Copyright Inria, contributors:
  Raphël Monat <raphael.monat@lip6.fr> (2019)

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

%{ open Tast
   open Parse_utils %}

%token<string> SYMBOL NAME INTEGER FLOAT

%token SLASH
%token NOM FIP
%token ENTREESP CONTROLESP RESULTATSP
%token ENTREESC CONTROLESC RESULTATSC
%token ENDSHARP

%token EOF

%type<Tast.test_file> test_file

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
  ENTREESC
  ec = list(variable_and_value)
  CONTROLESC
  cc = list(variable_and_value)
  RESULTATSC
  rc = list(variable_and_value)
  ENDSHARP { { nom; ep; cp; rp; ec; cc; rc } }
| EOF { assert false }

name:
| n = NAME { n }
| n = SYMBOL { n }

fip:
  FIP SLASH SYMBOL { }

variable_and_value:
| var = SYMBOL SLASH value = INTEGER  { (var, I (int_of_string value), mk_position $sloc) }
| var = SYMBOL SLASH value = FLOAT  { (var, F (float_of_string value), mk_position $sloc) }
