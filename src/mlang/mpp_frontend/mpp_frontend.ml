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

(* FIXME: scope is only to know variables stored in ctx by previous computation.
   We don't check that local variables read have been defined previously *)

open Mpp_ir

let to_scoped_var ?(scope = Input) (p : Mir.program)
    (var : Mpp_ast.var Pos.marked) : scoped_var =
  let var_s = Pos.unmark var in
  if String.uppercase_ascii var_s = var_s then
    (* we have an MBased variable *)
    Mbased (Mir.find_var_by_name p var, scope)
  else Local var_s

let to_mpp_callable (cname : string Pos.marked) (translated_names : string list)
    : mpp_callable =
  match Pos.unmark cname with
  | "present" -> Present
  | "abs" -> Abs
  | "cast" -> Cast
  | x ->
      if List.mem x translated_names then MppFunction x
      else
        Errors.raise_spanned_error
          (Format.sprintf "unknown callable %s" x)
          (Pos.get_position cname)

let rec to_mpp_expr (p : Mir.program) (translated_names : mpp_compute_name list)
    (scope : mpp_compute_name list) (e : Mpp_ast.expr) :
    mpp_expr * Mpp_ast.var list =
  let e', scope =
    match Pos.unmark e with
    | Constant i -> (Constant i, scope)
    | Variable v ->
        ( Variable
            (to_scoped_var
               ~scope:(if List.mem v scope then Output else Input)
               p (Pos.same_pos_as v e)),
          scope )
    | NbVarCategory l ->
        let cats = Mir.mast_to_catvars p.program_var_categories l in
        (Call (NbVarCat cats, []), [])
    | ExistsAttrWith (attr, value) ->
        (Call (ExistsAttrWithVal (attr, value), []), [])
    | ExistsAliases aliases ->
        let aliasMap =
          List.fold_left
            (fun res (name, pos) -> StrMap.add name pos res)
            StrMap.empty aliases
        in
        (Call (ExistsAliases aliasMap, []), [])
    | Unop (Minus, e) ->
        let e', scope = to_mpp_expr p translated_names scope e in
        (Unop (Minus, e'), scope)
    | CallRules (dom, args) ->
        let c' =
          let dom_id = Mast.DomainId.from_marked_list (Pos.unmark dom) in
          Rules dom_id
        in
        let new_scope = List.map Pos.unmark args in
        let args' = List.map (to_scoped_var p) args in
        (Call (c', args'), new_scope)
    | CallChain args ->
        let c', args =
          match args with
          | [] ->
              Errors.raise_spanned_error "Expected a chain to call"
                (Pos.get_position e)
          | chain :: args -> (Chain (Pos.unmark chain), args)
        in
        let new_scope = List.map Pos.unmark args in
        let args' = List.map (to_scoped_var p) args in
        (Call (c', args'), new_scope)
    | CallVerifs (dom, expr) ->
        let c' =
          let dom_id = Mast.DomainId.from_marked_list (Pos.unmark dom) in
          let filter = fst (to_mpp_expr p translated_names scope expr) in
          Verifs (dom_id, filter)
        in
        (Call (c', []), scope)
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

let rec to_mpp_stmt (p : Mir.program) (translated_names : string list)
    (scope : mpp_compute_name list) (stmt : Mpp_ast.stmt) :
    mpp_stmt * Mpp_ast.var list =
  let stmt', scope =
    match Pos.unmark stmt with
    | Assign (v, e) ->
        ( Assign
            ( to_scoped_var p (Pos.same_pos_as v e),
              fst @@ to_mpp_expr p translated_names scope e ),
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
    | Partition (attr, value, body) ->
        ( Partition (attr, value, to_mpp_stmts p translated_names ~scope body),
          scope )
  in
  (Pos.same_pos_as stmt' stmt, scope)

and to_mpp_stmts (p : Mir.program) (translated_names : mpp_compute_name list)
    ?(scope : mpp_compute_name list = []) (stmts : Mpp_ast.stmt list) :
    mpp_stmt list =
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
  {
    name;
    args = [];
    (* FIXME *)
    body = to_mpp_stmts p translated_names cdef.body;
  }

let cst_to_ast (c : Mpp_ast.program) (p : Mir.program) : Mpp_ir.mpp_program =
  List.rev @@ fst
  @@ List.fold_left
       (fun (mpp_acc, translated_names) cdef ->
         ( cdef_to_adef p translated_names cdef :: mpp_acc,
           cdef.Mpp_ast.name :: translated_names ))
       ([], []) c

let process (mpp_file : string) (p : Mir_interface.full_program) : mpp_program =
  Cli.debug_print "Parsing m++ file %s" mpp_file;
  let f = open_in mpp_file in
  let buf = Lexing.from_channel f in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = mpp_file };
  try
    let cst = Mpp_parser.file Mpp_lexer.next_token buf in
    close_in f;
    cst_to_ast cst p.program
  with Mpp_parser.Error ->
    Errors.raise_spanned_error "M++ syntax error"
      (Parse_utils.mk_position (buf.lex_start_p, buf.lex_curr_p))
