(* FIXME: scope is only to know variables stored in ctx by previous computation. We don't check that
   local variables read have been defined previously *)
open Mpp_ir

let to_scoped_var ?(scope = Input) (p : Mir.program) (var : Mpp_ast.var Pos.marked) : scoped_var =
  let var = Pos.unmark var in
  if String.uppercase_ascii var = var then
    (* we have an MBased variable *)
    Mbased (Mir.find_var_by_name p var, scope)
  else Local var

let to_mpp_callable (cname : string Pos.marked) (translated_names : string list) : mpp_callable =
  match Pos.unmark cname with
  | "present" -> Present
  | "abs" -> Abs
  | "cast" -> Cast
  | "exists_deposit_defined_variables" -> DepositDefinedVariables
  | "exists_taxbenefit_defined_variables" -> TaxbenefitDefinedVariables
  | "exists_taxbenefit_ceiled_variables" -> TaxbenefitCeiledVariables
  | "evaluate_program" -> Program
  | x ->
      if List.mem x translated_names then MppFunction x
      else
        Errors.raise_spanned_error (Format.sprintf "unknown callable %s" x) (Pos.get_position cname)

let rec to_mpp_expr (p : Mir.program) (translated_names : mpp_compute_name list)
    (scope : mpp_compute_name list) (e : Mpp_ast.expr) : mpp_expr * Mpp_ast.var list =
  let e', scope =
    match Pos.unmark e with
    | Constant i -> (Constant i, scope)
    | Variable v ->
        ( Variable
            (to_scoped_var
               ~scope:(if List.mem v scope then Output else Input)
               p (Pos.same_pos_as v e)),
          scope )
    | Unop (Minus, e) ->
        let e', scope = to_mpp_expr p translated_names scope e in
        (Unop (Minus, e'), scope)
    | Call (c, args) ->
        let c' = to_mpp_callable c translated_names in
        let new_scope = List.map Pos.unmark args in
        let args' = List.map (to_scoped_var p) args in
        (Call (c', args'), new_scope)
    | Binop (e1, b, e2) ->
        ( Binop
            ( fst @@ to_mpp_expr p translated_names scope e1,
              b,
              fst @@ to_mpp_expr p translated_names scope e2 ),
          scope )
  in
  (Pos.same_pos_as e' e, scope)

let to_mpp_filter (f : string Pos.marked) : mpp_filter =
  if Pos.unmark f = "var_is_taxbenefit" then VarIsTaxBenefit
  else
    Errors.raise_spanned_error
      (Format.asprintf "unknown filter %s" (Pos.unmark f))
      (Pos.get_position f)

let rec to_mpp_stmt (p : Mir.program) (translated_names : string list)
    (scope : mpp_compute_name list) (stmt : Mpp_ast.stmt) : mpp_stmt * Mpp_ast.var list =
  let stmt', scope =
    match Pos.unmark stmt with
    | Assign (v, e) ->
        ( Assign
            (to_scoped_var p (Pos.same_pos_as v e), fst @@ to_mpp_expr p translated_names scope e),
          scope )
    | Conditional (b, t, f) ->
        ( Conditional
            ( fst @@ to_mpp_expr p translated_names scope b,
              to_mpp_stmts p translated_names ~scope t,
              to_mpp_stmts p translated_names ~scope f ),
          scope )
    | Delete v -> (Delete (to_scoped_var p (Pos.same_pos_as v stmt)), scope)
    | Expr e ->
        let e', scope = to_mpp_expr p translated_names scope e in
        (Expr e', scope)
    | Partition (f, body) ->
        ( Partition
            (to_mpp_filter (Pos.same_pos_as f stmt), to_mpp_stmts p translated_names ~scope body),
          scope )
  in
  (Pos.same_pos_as stmt' stmt, scope)

and to_mpp_stmts (p : Mir.program) (translated_names : mpp_compute_name list)
    ?(scope : mpp_compute_name list = []) (stmts : Mpp_ast.stmt list) : mpp_stmt list =
  List.rev @@ fst
  @@ List.fold_left
       (fun (translated_stmts, scope) cstmt ->
         let stmt, scope = to_mpp_stmt p translated_names scope cstmt in
         (stmt :: translated_stmts, scope))
       ([], scope) stmts

let cdef_to_adef (p : Mir.program) (translated_names : mpp_compute_name list)
    (cdef : Mpp_ast.compute) : Mpp_ir.mpp_compute =
  let name = cdef.Mpp_ast.name in
  assert (not @@ List.mem name translated_names);
  { name; args = []; (* FIXME *)
                     body = to_mpp_stmts p translated_names cdef.body }

let cst_to_ast (c : Mpp_ast.program) (p : Mir.program) : Mpp_ir.mpp_program =
  List.rev @@ fst
  @@ List.fold_left
       (fun (mpp_acc, translated_names) cdef ->
         (cdef_to_adef p translated_names cdef :: mpp_acc, cdef.Mpp_ast.name :: translated_names))
       ([], []) c

let process (ompp_file : string option) (p : Mir_interface.full_program) : mpp_program option =
  match ompp_file with
  | None -> None
  | Some mpp_file -> (
      Cli.debug_print "Reading m++ file %s" mpp_file;
      let f = open_in mpp_file in
      let buf = Lexing.from_channel f in
      buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = mpp_file };
      try
        let cst = Mpp_parser.file Mpp_lexer.next_token buf in
        close_in f;
        Some (cst_to_ast cst p.program)
      with Mpp_parser.Error ->
        let b = Lexing.lexeme_start_p buf in
        let e = Lexing.lexeme_end_p buf in
        let l = b.pos_lnum in
        let fc = b.pos_cnum - b.pos_bol + 1 in
        let lc = e.pos_cnum - b.pos_bol + 1 in
        let () = Cli.error_print "File \"%s\", line %d, characters %d-%d:\n@." mpp_file l fc lc in
        None )
