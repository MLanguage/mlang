open M_ir

type ctx = { mutable indent : int; mutable is_newline : bool }

type t = { std : Com.print_std; std_fmt : Format.formatter; ctx_pr : ctx }

let fresh_ctx () = { indent = 0; is_newline = false }

let make std =
  match std with
  | Com.StdOut -> { std; std_fmt = Format.std_formatter; ctx_pr = fresh_ctx () }
  | Com.StdErr -> { std; std_fmt = Format.err_formatter; ctx_pr = fresh_ctx () }

let flush (pctx : t) =
  match pctx.std with
  | Com.StdOut -> ()
  | Com.StdErr -> Format.pp_print_flush pctx.std_fmt ()

let out_indent (pctx : t) =
  if pctx.ctx_pr.is_newline then (
    for _i = 1 to pctx.ctx_pr.indent do
      Format.fprintf pctx.std_fmt " "
    done;
    pctx.ctx_pr.is_newline <- false)

let raw (pctx : t) s =
  let len = String.length s in
  let rec aux = function
    | n when n >= len -> ()
    | n -> (
        match s.[n] with
        | '\n' ->
            Format.fprintf pctx.std_fmt "\n";
            flush pctx;
            pctx.ctx_pr.is_newline <- true;
            aux (n + 1)
        | c ->
            out_indent pctx;
            Format.fprintf pctx.std_fmt "%c" c;
            aux (n + 1))
  in
  aux 0

let set_indent (pctx : t) diff =
  pctx.ctx_pr.indent <- max 0 (pctx.ctx_pr.indent + diff)

let info (pctx : t) info (vsd : Com.variable_space) v =
  if not vsd.vs_by_default then (
    raw pctx (Pos.unmark vsd.vs_name);
    raw pctx ".");
  (* let _, v, _ = Context.get_var pctx.ctx None var in *)
  match info with
  | Com.Name -> raw pctx (Com.Var.name_str v)
  | Com.Alias -> raw pctx (Com.Var.alias_str v)

let string (pctx : t) s =
  raw pctx s;
  flush pctx

let access (pctx : t) i (vsd, var, _) =
  (* match get_access_var pctx.ctx acc with *)
  (* | Some (vsd, var, _) -> *)
  info pctx i vsd var;
  flush pctx
(* | None -> () *)

(* and pr_expr (pctx : pctx) mi ma e = *)
(*   pr_value pctx mi ma (evaluate_expr pctx.ctx e); *)
(*   pr_flush pctx *)

(* module Make (N : Mir_number.NumberInterface) = struct *)

(*   let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter) *)
(*         (x : N.t value) = *)
(*     match x with *)
(*     | Undefined -> Com.format_literal fmt Com.Undefined *)
(*     | Number x -> N.format_prec_t mi ma fmt x *)

(*   let value (pctx : pctx) mi ma value = *)
(*     raw pctx (Pp.spr "%a" (format_value_prec mi ma) value) *)

(*   let expr ~eval (pctx : pctx) mi ma e = *)
(*     value pctx mi ma (eval pctx.ctx e); *)
(*     flush pctx *)
(* end *)
