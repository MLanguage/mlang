(* Copyright (C) 2019-2021 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr> Raphaël Monat <raphael.monat@lip6.fr>

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

let create_combined_program (m_program : Mir.program)
    (mpp_function_to_extract : string) : Bir.program =
  try
    let targets =
      Mir.TargetMap.fold
        (fun n t targets ->
          Mir.TargetMap.add n
            Bir.
              {
                file = t.Mir.target_file;
                tmp_vars = t.Mir.target_tmp_vars;
                stmts = t.Mir.target_prog;
              }
            targets)
        m_program.program_targets Mir.TargetMap.empty
    in
    if not (Mir.TargetMap.mem mpp_function_to_extract targets) then
      Errors.raise_error
        (Format.asprintf "M target %s not found in M file!"
           mpp_function_to_extract);
    {
      targets;
      main_function = mpp_function_to_extract;
      mir_program = m_program;
    }
  with Bir_interpreter.FloatDefInterp.RuntimeError (r, _ctx) ->
    Bir_interpreter.FloatDefInterp.raise_runtime_as_structured r
