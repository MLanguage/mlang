(*
Copyright Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr> (2019)

This software is a computer program whose purpose is to compile and analyze
programs written in the M langage, created by thge DGFiP.

This software is governed by the CeCILL-C license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-C
license as circulated by CEA, CNRS and INRIA at the following URL
http://www.cecill.info.

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-C license and that you accept its terms.
*)

open Cfg

type ctx = {
  ctx_generic_table_index: expression Ast.marked option;
  ctx_program : program;
  ctx_lvar_mapping : LocalVariable.t LocalVariableMap.t
}

let empty_ctx (p: program) : ctx = {
  ctx_generic_table_index = None ;
  ctx_program = p;
  ctx_lvar_mapping = LocalVariableMap.empty
}

let lvar_mapping : LocalVariable.t Cfg.LocalVariableMap.t ref =
  ref LocalVariableMap.empty

let rec inline_vars_in_expr
    (ctx: ctx)
    (inlined_vars: unit VariableMap.t)
    (e: expression Ast.marked)
  : expression Ast.marked = match Ast.unmark e with
  | Comparison (op, e1, e2) ->
    Ast.same_pos_as
      (Comparison
         (op,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr ctx inlined_vars e2
         )) e
  | Binop(op, e1, e2) ->
    Ast.same_pos_as
      (Binop
         (op,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr ctx inlined_vars e2
         )) e
  | Unop (op, e') ->
    Ast.same_pos_as
      (Unop
         (op,
          inline_vars_in_expr ctx inlined_vars e'
         )) e
  | Conditional (e1, e2, e3) ->
    Ast.same_pos_as
      (Conditional
         (
           inline_vars_in_expr ctx inlined_vars e1,
           inline_vars_in_expr ctx inlined_vars e2,
           inline_vars_in_expr ctx inlined_vars e3
         )) e
  | FunctionCall (func, args) ->
    Ast.same_pos_as
      (FunctionCall
         (func,
          List.map (fun arg -> inline_vars_in_expr ctx inlined_vars arg) args
         )) e
  | Literal _ | Error -> e
  | GenericTableIndex -> begin match ctx.ctx_generic_table_index with
      | None -> e
      | Some gen_index -> gen_index
    end
  | LocalLet (lvar, e1, e2) ->
    let new_lvar =  LocalVariable.new_var () in
    let new_ctx =
      { ctx with ctx_lvar_mapping =
                   LocalVariableMap.add lvar new_lvar ctx.ctx_lvar_mapping }
    in
    lvar_mapping := LocalVariableMap.add new_lvar lvar !lvar_mapping;
    Ast.same_pos_as
      (LocalLet
         (new_lvar,
          inline_vars_in_expr ctx inlined_vars e1,
          inline_vars_in_expr new_ctx inlined_vars e2
         )) e
  | Var var -> if VariableMap.mem var inlined_vars then
      begin match (VariableMap.find var ctx.ctx_program).var_definition with
        | SimpleVar new_e -> inline_vars_in_expr ctx inlined_vars new_e
        | InputVar -> e
        | TableVar _ -> assert false (* should not happen *)
      end else
      e
  | LocalVar lvar ->
    Ast.same_pos_as (LocalVar (LocalVariableMap.find lvar ctx.ctx_lvar_mapping)) e
  | Index (var, index) ->
    let new_index = inline_vars_in_expr ctx inlined_vars index in
    if VariableMap.mem (Ast.unmark var) inlined_vars then
      begin match (VariableMap.find (Ast.unmark var) ctx.ctx_program).var_definition with
        | SimpleVar _  | InputVar -> assert false (* should not happen *)
        | TableVar (size, table_def) ->
          begin match table_def with
            | IndexGeneric new_e ->
              let new_index_var = LocalVariable.new_var () in
              Ast.same_pos_as
                (LocalLet(new_index_var, new_index, inline_vars_in_expr
                            { ctx with ctx_generic_table_index =
                                         Some (Ast.same_pos_as (LocalVar new_index_var) index) }
                            inlined_vars
                            new_e)) index
            | IndexTable indexes_def -> match Ast.unmark new_index with
              | Literal (Int i) when i < size ->
                let correct_def = IndexMap.find i indexes_def in
                inline_vars_in_expr ctx inlined_vars correct_def
              | _ ->
                raise (Errors.TypeError
                         (Errors.Inlining
                            (Printf.sprintf
                               "cannot inline access to table %s in expression %s because the variable \
                                is not defined generically and the accessing index cannot be \
                                computed at compile time"
                               (Ast.unmark (Ast.unmark var).Variable.name)
                               (Format_ast.format_position (Ast.get_position e))
                            )))
          end
      end else
      e


let inline_vars
    (inlined_vars:unit VariableMap.t)
    (typing: Typechecker.typ_info)
    (p: program)
  : program * Typechecker.typ_info =
  lvar_mapping := LocalVariableMap.empty;
  let new_program = VariableMap.fold (fun var def acc ->
      if VariableMap.mem var inlined_vars then
        acc
      else begin
        let new_def = match def.var_definition with
          | InputVar -> InputVar
          | SimpleVar e -> SimpleVar (inline_vars_in_expr (empty_ctx p) inlined_vars e)
          | TableVar (size, def) -> begin match def with
              | IndexGeneric e ->
                TableVar (size, IndexGeneric (inline_vars_in_expr (empty_ctx p) inlined_vars e))
              | IndexTable es ->
                TableVar (size, IndexTable (IndexMap.map (fun e ->
                    inline_vars_in_expr (empty_ctx p) inlined_vars e
                  ) es))
            end
        in
        VariableMap.add var { def with var_definition = new_def } acc
      end
    ) p VariableMap.empty in
  let new_typing =
    { typing with
      Typechecker.typ_info_local_var = LocalVariableMap.mapi (fun new_lvar old_lvar ->
          try LocalVariableMap.find old_lvar typing.Typechecker.typ_info_local_var with
          | Not_found -> begin
              assert false (* TODO: fix bug *)
            end
        ) !lvar_mapping
    }
  in
  (new_program, new_typing)
