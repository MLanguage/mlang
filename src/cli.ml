(*
  Copyright 2018 Denis Merigoux and INRIA

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

(** Command-line interface helpers *)

let source_files : string list ref = ref []
let dep_graph_file : string ref = ref "dep_graph.dot"
let verify_flag = ref false
let debug_flag = ref false

let debug_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "
let error_marker () = ANSITerminal.eprintf [ANSITerminal.Bold; ANSITerminal.red] "[ERROR] "
let warning_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.yellow] "[WARNING] "

let debug_print (s: string) =
  if !debug_flag then begin
    debug_marker ();
    Printf.printf "%s\n" s;
    flush stdout;
    flush stdout
  end

let error_print (s: string) =
  error_marker ();
  Printf.eprintf "%s\n" s;
  flush stdout;
  flush stdout

let warning_print (s: string) =
  warning_marker ();
  Printf.printf "%s\n" s;
  flush stdout;
  flush stdout
