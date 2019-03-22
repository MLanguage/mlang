(** Command-line interface helpers *)

let source_files : string list ref = ref []
let verify_flag = ref false
let debug_flag = ref false

let debug_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "
let error_marker () = ANSITerminal.eprintf [ANSITerminal.Bold; ANSITerminal.red] "[ERROR] "


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
