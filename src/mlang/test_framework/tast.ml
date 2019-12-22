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

type literal =
  | I of int
  | F of float

type var_values = (string * literal * Pos.position) list

type test_file =
  { nom : string;
    ep : var_values;
    cp : var_values;
    rp : var_values;
    ec : var_values;
    cc : var_values;
    rc : var_values }
