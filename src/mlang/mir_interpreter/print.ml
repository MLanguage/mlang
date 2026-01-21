open Types

module Make (N : M_ir.Mir_number.NumberInterface) = struct
  let _format_value (fmt : Format.formatter) (x : N.t value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_t fmt x

  let format_value_prec (mi : int) (ma : int) (fmt : Format.formatter)
      (x : N.t value) =
    match x with
    | Undefined -> Com.format_literal fmt Com.Undefined
    | Number x -> N.format_prec_t mi ma fmt x

  let fresh std ctx =
    match std with
    | Com.StdOut ->
        { std; ctx; std_fmt = Format.std_formatter; ctx_pr = ctx.ctx_pr_out }
    | Com.StdErr ->
        { std; ctx; std_fmt = Format.err_formatter; ctx_pr = ctx.ctx_pr_err }

  let flush (pctx : 'a pctx) =
    match pctx.std with
    | Com.StdOut -> ()
    | Com.StdErr -> Format.pp_print_flush pctx.std_fmt ()

  let pr_out_indent (pctx : 'a pctx) =
    if pctx.ctx_pr.is_newline then (
      for _i = 1 to pctx.ctx_pr.indent do
        Format.fprintf pctx.std_fmt " "
      done;
      pctx.ctx_pr.is_newline <- false)

  let pr_raw (pctx : 'a pctx) s =
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
              pr_out_indent pctx;
              Format.fprintf pctx.std_fmt "%c" c;
              aux (n + 1))
    in
    aux 0

  let pr_set_indent (pctx : 'a pctx) diff =
    pctx.ctx_pr.indent <- max 0 (pctx.ctx_pr.indent + diff)

  let value (pctx : 'a pctx) mi ma value =
    pr_raw pctx (Pp.spr "%a" (format_value_prec mi ma) value)

  let string (pctx : 'a pctx) s =
    pr_raw pctx s;
    flush pctx

  let access (pctx : 'a pctx) pinfo vsd var =
    if not vsd.Com.vs_by_default then (
      pr_raw pctx (Pos.unmark vsd.vs_name);
      pr_raw pctx ".");
    match pinfo with
    | Com.Name -> pr_raw pctx (Com.Var.name_str var)
    | Com.Alias -> pr_raw pctx (Com.Var.alias_str var)

  and indent (pctx : 'a pctx) = function
    | Undefined -> ()
    | Number diff -> pr_set_indent pctx (Int64.to_int @@ N.to_int diff)
end
