(*
Copyright Inria, contributors:
  Raphël Monat <raphael.monat@lip6.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by the DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

%{ open Tast
   open Parse_utils %}

%token<string> SYMBOL
%token<string> INTEGER
%token<string> FLOAT

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
| NOM nom = SYMBOL
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


fip:
  FIP SLASH SYMBOL { }

variable_and_value:
| var = SYMBOL SLASH value = INTEGER  { (var, I (int_of_string value), mk_position $sloc) }
| var = SYMBOL SLASH value = FLOAT  { (var, F (float_of_string value), mk_position $sloc) }
