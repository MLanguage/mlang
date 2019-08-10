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

{
open Lexing
open Tparser
open Errors
}

rule token = parse
| [' ' '\t'] (* also ignore newlines, not only whitespace and tabs *)
  { token lexbuf }
| '*' [^ '\n']* '\n' (* ignore comments *)
  { new_line lexbuf; token lexbuf }
| '\n' | "\r\n"
  { new_line lexbuf; token lexbuf}
| "/"
  { SLASH }
| "#NOM"
  { NOM }
| "#FIP"
  { FIP }
| "#ENTREES-PRIMITIF"
  { ENTREESP }
| "#CONTROLES-PRIMITIF"
  { CONTROLESP }
| "#RESULTATS-PRIMITIF"
  { RESULTATSP }
| "#ENTREES-CORRECTIF"
  { ENTREESC }
| "#CONTROLES-CORRECTIF"
  { CONTROLESC }
| "#RESULTATS-CORRECTIF"
  { RESULTATSC }
| "##"
  { ENDSHARP }
| '-'? ['0' - '9']+ as i
  { INTEGER i }
| '-'? ['0' - '9']+ '.' ['0' - '9']* as f
  { FLOAT f }
| ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as s
  { SYMBOL s }
| eof
  { EOF }
| _
  { raise (LexingError (lexer_error lexbuf)) }
