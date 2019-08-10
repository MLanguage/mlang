(*
Copyright (C) 2019 Inria, contributors:
  Denis Merigoux <denis.merigoux@inria.fr>
  Raphël Monat <raphael.monat@lip6.fr>

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
let check (program: Mvg.program) : unit =
  let v = object
    inherit [_] Execution_order.program_iter
    method! visit_Literal _ l =
      match l with
      | Undefined ->
        raise (Errors.TypeError (Typing ("undef still in " ^ (Ast.format_position @@ !Mvg.current_visitor_pos))))
      | _ -> ()
  end in
  v#visit_program () program
