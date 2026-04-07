(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

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

type structured_msg = { msg : string; spans : (string option * Pos.t) list }

let make ?(spans = []) msg = { msg; spans }

let fmake ?spans fmt = Format.kasprintf (make ?spans) fmt

module type S = sig
  val debug_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val var_info_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val error_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val warning_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val result_print : ('a, Format.formatter, unit, unit) format4 -> 'a

  val format : Format.formatter -> structured_msg -> unit

  val create_progress_bar : string -> (string -> unit) * (string -> unit)
end

module ANSITerminal = struct
  (** {2 Markers} *)

  (** Prints [[INFO]] in blue on the terminal standard output *)
  let var_info_marker () =
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.blue ] "[VAR INFO] "

  let time : float ref = ref (Unix.gettimeofday ())

  let initial_time : float ref = ref (Unix.gettimeofday ())

  let time_marker () =
    let new_time = Unix.gettimeofday () in
    let old_time = !time in
    time := new_time;
    let delta = (new_time -. old_time) *. 1000. in
    if delta > 100. then
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.black ]
        "[TIME] %.0f ms\n" delta

  let format_with_style (styles : ANSITerminal.style list)
      (str : ('a, unit, string) format) =
    if true (* can depend on a stylr flag *) then
      ANSITerminal.sprintf styles str
    else Printf.sprintf str

  (** Prints [[DEBUG]] in purple on the terminal standard output as well as
      timing since last debug *)
  let debug_marker () =
    if !Config.display_time then time_marker ();
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.magenta ] "[DEBUG] "

  (** Prints [[ERROR]] in red on the terminal error output *)
  let error_marker () =
    ANSITerminal.eprintf [ ANSITerminal.Bold; ANSITerminal.red ] "[ERROR] "

  (** Prints [[WARNING]] in yellow on the terminal standard output *)
  let warning_marker () =
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "[WARNING] "

  (** Prints [[RESULT]] in green on the terminal standard output *)
  let result_marker () =
    ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.green ] "[RESULT] "

  let clocks =
    Array.of_list [ "🕛"; "🕐"; "🕑"; "🕒"; "🕓"; "🕔"; "🕕"; "🕖"; "🕗"; "🕘"; "🕙"; "🕚" ]

  (** Prints [[🕛]] in blue on the terminal standard output *)
  let clock_marker i =
    let new_time = Unix.gettimeofday () in
    let initial_time = !initial_time in
    let delta = new_time -. initial_time in
    ANSITerminal.printf
      [ ANSITerminal.Bold; ANSITerminal.blue ]
      "[%s  %.1f s] "
      clocks.(i mod Array.length clocks)
      delta

  let create_progress_bar (task : string) : (string -> unit) * (string -> unit)
      =
    if !Config.no_nondet_display then (ignore, ignore)
    else
      let step_ticks = 5 in
      let ticks = ref 0 in
      let msg = ref task in
      let stop = ref false in
      let timer () =
        while true do
          if !stop then Thread.exit ();
          ticks := !ticks + 1;
          if !Config.display_time then clock_marker (!ticks / step_ticks);
          Format.printf "%s" !msg;
          flush_all ();
          flush_all ();
          ANSITerminal.erase ANSITerminal.Below;
          ANSITerminal.move_bol ();
          Unix.sleepf 0.05
        done
      in
      let _ = Thread.create timer () in
      ( (fun current_progress_msg ->
          msg := Format.sprintf "%s: %s" task current_progress_msg),
        fun finish_msg ->
          stop := true;
          result_marker ();
          Format.printf "%s: %s" task finish_msg;
          ANSITerminal.erase ANSITerminal.Below;
          ANSITerminal.move_bol ();
          Format.printf "\n";
          time_marker () )

  (**{2 Printers}*)

  let debug_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    if !Config.debug_flag then
      Format.kasprintf
        (fun str ->
          debug_marker ();
          Format.printf "%s\n@?" str)
        ppf
    else Format.ifprintf Format.std_formatter ppf

  let var_info_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    if !Config.var_info_flag then
      Format.kasprintf
        (fun str ->
          var_info_marker ();
          Format.printf "%s@." str)
        ppf
    else Format.ifprintf Format.std_formatter ppf

  let error_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    Format.kasprintf
      (fun str ->
        error_marker ();
        Format.eprintf "%s@." str)
      ppf

  let warning_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    if !Config.warning_flag then
      Format.kasprintf
        (fun str -> Format.printf "%a%s@." (fun _ -> warning_marker) () str)
        ppf
    else Format.ifprintf Format.std_formatter ppf

  let result_print ppf =
    ANSITerminal.erase ANSITerminal.Eol;
    Format.kasprintf
      (fun str -> Format.printf "%a%s@." (fun _ -> result_marker) () str)
      ppf

  let indent_number (s : string) : int =
    try
      let rec aux (i : int) = if s.[i] = ' ' then aux (i + 1) else i in
      aux 0
    with Invalid_argument _ -> String.length s

  let retrieve_loc_text (pos : Pos.t) : string =
    let filename = Pos.get_file pos in
    let blue_style = [ ANSITerminal.Bold; ANSITerminal.blue ] in
    if filename = "" then "No position information"
    else
      let sline = Pos.get_start_line pos in
      let eline = Pos.get_end_line pos in
      let oc, input_line_opt =
        try
          if filename == Dgfip_m.internal_m then
            let input_line_opt : unit -> string option =
              let curr = ref 0 in
              let src = Dgfip_m.declarations in
              let lng = String.length src in
              let rec new_curr () =
                if !curr < lng then
                  if src.[!curr] = '\n' then (
                    let res = !curr in
                    incr curr;
                    Some res)
                  else (
                    incr curr;
                    new_curr ())
                else None
              in
              function
              | () -> (
                  let p0 = !curr in
                  match new_curr () with
                  | None -> None
                  | Some p1 ->
                      Some (String.sub Dgfip_m.declarations p0 (p1 - p0)))
            in
            (None, input_line_opt)
          else
            let ocf = open_in filename in
            let input_line_opt () : string option =
              try Some (input_line ocf) with End_of_file -> None
            in
            (Some ocf, input_line_opt)
        with Sys_error _ ->
          error_print "File not found for displaying position : %S" filename;
          failwith "Pos error"
      in
      let print_matched_line (line : string) (line_no : int) : string =
        let line_indent = indent_number line in
        let error_indicator_style = [ ANSITerminal.red; ANSITerminal.Bold ] in
        let line_start_col =
          if line_no = sline then Pos.get_start_column pos else 1
        in
        let line_end_col =
          if line_no = eline then Pos.get_end_column pos
          else String.length line + 1
        in
        let line_length = String.length line + 1 in
        line
        ^
        if line_no >= sline && line_no <= eline then
          "\n"
          ^
          if line_no = sline && line_no = eline then
            format_with_style error_indicator_style "%*s" (line_end_col - 1)
              (String.make (line_end_col - line_start_col) '^')
          else if line_no = sline && line_no <> eline then
            format_with_style error_indicator_style "%*s" (line_length - 1)
              (String.make (line_length - line_start_col) '^')
          else if line_no <> sline && line_no <> eline then
            format_with_style error_indicator_style "%*s%s" line_indent ""
              (String.make (line_length - line_indent) '^')
          else if line_no <> sline && line_no = eline then
            format_with_style error_indicator_style "%*s%*s" line_indent ""
              (line_end_col - 1 - line_indent)
              (String.make (line_end_col - line_indent) '^')
          else assert false (* should not happen *)
        else ""
      in
      let include_extra_count = 0 in
      let rec get_lines (n : int) : string list =
        match input_line_opt () with
        | Some line ->
            if n < sline - include_extra_count then get_lines (n + 1)
            else if
              n >= sline - include_extra_count
              && n <= eline + include_extra_count
            then print_matched_line line n :: get_lines (n + 1)
            else []
        | None -> []
      in
      let pos_lines = get_lines 1 in
      let spaces = int_of_float (log10 (float_of_int eline)) + 1 in
      (match oc with Some ocf -> close_in ocf | _ -> ());
      format_with_style blue_style "%*s--> %s\n%s" spaces "" filename
        (Cli.add_prefix_to_each_line
           (Printf.sprintf "\n%s" (String.concat "\n" pos_lines))
           (fun i ->
             let cur_line = sline - include_extra_count + i - 1 in
             if
               cur_line >= sline
               && cur_line <= sline + (2 * (eline - sline))
               && cur_line mod 2 = sline mod 2
             then
               format_with_style blue_style "%*d | " spaces
                 (sline + ((cur_line - sline) / 2))
             else if cur_line >= sline - include_extra_count && cur_line < sline
             then format_with_style blue_style "%*d | " spaces cur_line
             else if
               cur_line
               <= sline + (2 * (eline - sline)) + 1 + include_extra_count
               && cur_line > sline + (2 * (eline - sline)) + 1
             then
               format_with_style blue_style "%*d | " spaces
                 (cur_line - (eline - sline + 1))
             else format_with_style blue_style "%*s | " spaces ""))

  let format fmt { msg; spans } =
    Format.fprintf fmt "%s%s%s%s" msg
      (if spans = [] then "" else "\n\n")
      (String.concat "\n\n"
         (List.map
            (fun (msg, pos) ->
              Printf.sprintf "%s%s"
                (match msg with None -> "" | Some msg -> msg ^ "\n")
                (retrieve_loc_text pos))
            spans))
      (if spans = [] then "" else "\n")
end

module GNU = struct
  include ANSITerminal

  let format fmt { msg; spans } =
    if spans = [] then Format.fprintf fmt "%s\n" msg
    else
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.pp_print_newline fmt ())
        (fun fmt (pos_msg, pos) ->
          Format.fprintf fmt "%a: %s %a\n" Pos.format_gnu pos msg
            (fun fmt pos_msg ->
              match pos_msg with
              | None -> ()
              | Some pos_msg -> Format.fprintf fmt "[%s]" pos_msg)
            pos_msg)
        fmt spans
end

let logger_select () =
  match !Config.message_format with
  | GNU -> (module GNU : S)
  | ANSI -> (module ANSITerminal : S)

let error_print ppf =
  let module L = (val logger_select ()) in
  L.error_print ppf

let warning_print ppf =
  let module L = (val logger_select ()) in
  L.warning_print ppf

let debug_print ppf =
  let module L = (val logger_select ()) in
  L.debug_print ppf

let result_print ppf =
  let module L = (val logger_select ()) in
  L.result_print ppf

let error_str : string -> unit = error_print "%s"

let warning_str : string -> unit = warning_print "%s"

let debug_str : string -> unit = debug_print "%s"

let result_str : string -> unit = result_print "%s"

let create_progress_bar ppf =
  let module L = (val logger_select ()) in
  L.create_progress_bar ppf

let format_structured_message ppf =
  let module L = (val logger_select ()) in
  L.format ppf
