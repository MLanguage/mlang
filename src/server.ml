open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Errors = Mlang.Errors
module Json = Yojson.Safe.Util

let asf = Format.asprintf

type file_assoc = string * string [@@deriving yojson]

type filemap = file_assoc list [@@deriving yojson]

module Msg = struct
  type 'a msg_result = Ok of 'a | Err of string

  let yojson_of_msg_result aconv t =
    match t with
    | Err s -> `Assoc [ ("ok", `Bool false); ("error", `String s) ]
    | Ok s -> `Assoc [ ("ok", `Bool true); ("value", aconv s) ]

  let msg_result_of_yojson _ _ = assert false

  module Out = struct
    type parsing = { id : string; payload : string } [@@deriving yojson]

    let parsing name =
      let m = { id = "parsing"; payload = name } in
      let json = yojson_of_parsing m in
      Yojson.Safe.to_string json

    type end_parsing = { id : string; payload : string } [@@deriving yojson]

    let end_parsing name =
      let m = { id = "parsing-end"; payload = name } in
      yojson_of_end_parsing m |> Yojson.Safe.to_string

    type 'a run_ret = { id : string; payload : 'a msg_result }
    [@@deriving yojson]

    let run_ret (payload : 'a msg_result) =
      let m = { id = "run-ret"; payload } in
      yojson_of_run_ret yojson_of_string m |> Yojson.Safe.to_string
  end

  module In = struct
    type run = {
      filemap : filemap;
      application : string;
      target : string;
      irj_contents : string;
    }
    [@@deriving yojson]
  end
end

let make_callbacks socket : ServerDriver.callbacks =
  let start_parsing name =
    let msg = Msg.Out.parsing name in
    Dream.send socket msg
  in
  let end_parsing name =
    let msg = Msg.Out.end_parsing name in
    Dream.send socket msg
  in
  { start_parsing; end_parsing }

let parse_files socket payload =
  let filemap = filemap_of_yojson payload in
  let callbacks = make_callbacks socket in
  ServerDriver.parse_files callbacks filemap

let run socket payload =
  let payload : Msg.In.run = Msg.In.run_of_yojson payload in
  let callbacks = make_callbacks socket in
  try
    let%lwt dbg_info =
      ServerDriver.run callbacks payload.filemap payload.irj_contents
        payload.target payload.application
    in
    Format.printf "%s@." dbg_info;
    let msg = Msg.Out.run_ret @@ Ok dbg_info in
    Dream.send socket msg
  with
  | Errors.StructuredError (a, b, _) ->
      let msg = asf "%a@." Errors.format_structured_error (a, b) in
      let msg = Msg.Out.run_ret @@ Err msg in
      Dream.send socket msg
  | e ->
      let msg = Printexc.to_string e in
      let msg = Msg.Out.run_ret @@ Err msg in
      let%lwt () = Dream.send socket msg in
      raise e

let () =
  Dream.run ~port:4242
  @@ Dream.router
       [
         Dream.get "/websocket" (fun _ ->
             Dream.websocket (fun socket ->
                 let rec loop () =
                   match%lwt Dream.receive socket with
                   | Some msg ->
                       print_endline msg;
                       let json = Yojson.Safe.from_string msg in
                       let id = Json.member "id" json in
                       let payload = Json.member "payload" json in
                       let%lwt () =
                         match id with
                         | `Null -> Lwt.return ()
                         | `String "parse-files" ->
                             Lwt.return @@ ignore @@ parse_files socket payload
                         | `String "run" ->
                             Lwt.return @@ ignore @@ run socket payload
                         | `String id ->
                             Dream.send socket
                               (Format.asprintf
                                  {|{"msg": "unkown message '%s'"}|} id)
                         | _ -> assert false
                       in
                       loop ()
                   | _ -> Dream.close_websocket socket
                 in
                 loop ()));
       ]
