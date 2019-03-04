let source_files : string list ref = ref []
let verify_flag = ref false
let debug_flag = ref false

let debug_print (s: string) =
  if !debug_flag then begin
    Printf.printf "%s\n" s;
    flush stdout;
    flush stdout
  end
