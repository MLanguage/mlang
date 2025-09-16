(* Copyright (C) 2019 Inria, contributor: David Declerck
   <david.declerck@ocamlpro.com> This program is free software: you can
   redistribute it and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version. This program is distributed
   in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
   the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU General Public License for more details. You should have received
   a copy of the GNU General Public License along with this program. If not, see
   <https://www.gnu.org/licenses/>. *)

open Cmdliner

let income_year = Arg.(value & opt int 1991 & info [ "m" ] ~doc:"Income year")

let iliad_pro =
  Arg.(
    value & flag
    & info [ "R" ] ~doc:"Set application to both \"iliad\" and \"pro\"")

let cfir = Arg.(value & flag & info [ "U" ] ~doc:"Set application to \"cfir\"")

let batch =
  Arg.(
    value
    & opt (some int) None
    & info [ "b" ]
        ~doc:"Set application to \"batch\" (b0 = normal, b1 = with EBCDIC sort)")

let primitive_only =
  Arg.(value & flag & info [ "P" ] ~doc:"Primitive calculation only")

let extraction =
  Arg.(value & flag & info [ "X" ] ~doc:"Generate global extraction")

let separate_controls =
  Arg.(value & flag & info [ "S" ] ~doc:"Generate separate controls")

let immediate_controls =
  Arg.(value & flag & info [ "I" ] ~doc:"Generate immediate controls")

let overlays = Arg.(value & flag & info [ "o" ] ~doc:"Generate overlays")

let optim_min_max =
  Arg.(
    value & flag
    & info [ "O" ] ~doc:"Optimize generated code (inline min_max function)")

let register =
  Arg.(value & flag & info [ "r" ] ~doc:"Pass TGV pointer as register in rules")

let short =
  Arg.(value & flag & info [ "s" ] ~doc:"Strip comments from generated output")

let output_labels =
  Arg.(value & flag & info [ "D" ] ~doc:"Generate labels for output variables")

let debug = Arg.(value & flag & info [ "g" ] ~doc:"Generate for test (debug)")

let nb_debug_c =
  Arg.(value & opt int 0 & info [ "k" ] ~doc:"Number of debug files")

let trace = Arg.(value & flag & info [ "t" ] ~doc:"Generate trace code")

let ticket =
  Arg.(value & flag & info [ "L" ] ~doc:"Generate calls to ticket function")

let colored_output =
  Arg.(value & flag & info [ "Z" ] ~doc:"Colored output in chainings")

let cross_references =
  Arg.(value & flag & info [ "x" ] ~doc:"Generate cross references")

let dgfip_t f =
  Term.(
    const f $ income_year $ iliad_pro $ cfir $ batch $ primitive_only
    $ extraction $ separate_controls $ immediate_controls $ overlays
    $ optim_min_max $ register $ short $ output_labels $ debug $ nb_debug_c
    $ trace $ ticket $ colored_output $ cross_references)

let info =
  let doc = "DGFiP-specific options for Mlang." in
  let man =
    [
      `S Manpage.s_synopsis;
      `P "$(b,mlang --dgfip_options=)[OPTION],...";
      `S Manpage.s_description;
      `P
        "This option allows to specify options specific to the Mlang DGFiP \
         backend. Each option should be separated by a comma, without spaces.";
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Raphael Monat <raphael.monat@lip6.fr>";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://github.com/MLanguage/mlang/issues";
    ]
  in
  Cmd.info "mlang --dgfip_options" ~doc ~man

let handler ~(application_names : string list) (income_year : int)
    (iliad_pro : bool) (cfir : bool) (batch : int option)
    (primitive_only : bool) (extraction : bool) (separate_controls : bool)
    (immediate_controls : bool) (overlays : bool) (optim_min_max : bool)
    (register : bool) (short : bool) (output_labels : bool) (debug : bool)
    (nb_debug_c : int) (trace : bool) (ticket : bool) (colored_output : bool)
    (cross_references : bool) : Config.Dgfip_options.flags =
  let has_iliad = List.mem "iliad" application_names in
  let has_pro = List.mem "pro" application_names in
  Config.Dgfip_options.
    {
      (* iliad, pro, (GP) *)
      annee_revenu = income_year;
      flg_correctif = not primitive_only;
      flg_iliad =
        ((iliad_pro && not cfir) || has_iliad) && not (Option.is_some batch);
      flg_pro = (has_pro || iliad_pro) && not cfir;
      flg_cfir = cfir && not iliad_pro;
      flg_gcos = Option.is_some batch && (not iliad_pro) && not cfir;
      flg_tri_ebcdic = (match batch with Some 1 -> true | _ -> false);
      flg_short = short;
      flg_register = register;
      flg_optim_min_max = optim_min_max;
      flg_extraction = extraction;
      flg_genere_libelle_restituee = output_labels;
      flg_controle_separe = separate_controls;
      flg_controle_immediat = immediate_controls;
      flg_overlays = overlays;
      flg_colors = colored_output;
      flg_ticket = ticket;
      flg_trace = trace;
      flg_debug = debug || trace;
      nb_debug_c;
      xflg = cross_references;
    }

let process_dgfip_options ~application_names options =
  let options = Array.of_list ("mlang" :: options) in
  let cmd = Cmd.v info (dgfip_t (handler ~application_names)) in
  let res = Cmd.eval_value ~argv:options cmd in
  match res with
  | Ok res -> ( match res with `Ok res -> Some res | _ -> None)
  | _ -> None
