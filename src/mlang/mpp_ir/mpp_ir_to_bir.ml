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

type translation_ctx = {
  new_variables : Bir.variable StrMap.t;
  variables_used_as_inputs : Mir.VariableDict.t;
  used_rule_domains : Mast.DomainIdSet.t;
  used_chainings : Mast.ChainingSet.t;
  verif_seen : bool;
}

let empty_translation_ctx : translation_ctx =
  {
    new_variables = StrMap.empty;
    variables_used_as_inputs = Mir.VariableDict.empty;
    used_rule_domains = Mast.DomainIdSet.empty;
    used_chainings = Mast.ChainingSet.empty;
    verif_seen = false;
  }

let ctx_join ctx1 ctx2 =
  {
    new_variables =
      StrMap.union
        (fun _ v1 v2 ->
          assert (Bir.compare_variable v1 v2 = 0);
          Some v2)
        ctx1.new_variables ctx2.new_variables;
    variables_used_as_inputs =
      Mir.VariableDict.union ctx1.variables_used_as_inputs
        ctx2.variables_used_as_inputs;
    used_rule_domains =
      Mast.DomainIdSet.union ctx1.used_rule_domains ctx2.used_rule_domains;
    used_chainings =
      Mast.ChainingSet.union ctx1.used_chainings ctx2.used_chainings;
    verif_seen = ctx1.verif_seen || ctx2.verif_seen;
  }

let translate_to_binop (b : Mpp_ast.binop) : Mast.binop =
  match b with And -> And | Or -> Or | _ -> assert false

let translate_to_compop (b : Mpp_ast.binop) : Mast.comp_op =
  match b with
  | Gt -> Gt
  | Gte -> Gte
  | Lt -> Lt
  | Lte -> Lte
  | Eq -> Eq
  | Neq -> Neq
  | _ -> assert false

let generate_input_condition (crit : Mir.Variable.t -> bool)
    (p : Mir_interface.full_program) (pos : Pos.t) =
  (* this might do wierd thing iif all variables to check are not "saisie" since
     the filter may find duplicates used in different contexts *)
  let variables_to_check =
    Bir.set_from_mir_dict Bir.default_tgv
    @@ Mir.VariableDict.filter (fun _ var -> crit var) p.program.program_vars
  in
  let mk_call_present x =
    (Mir.FunctionCall (PresentFunc, [ (Mir.Var x, pos) ]), pos)
  in
  let mk_or e1 e2 = (Mir.Binop ((Or, pos), e1, e2), pos) in
  let mk_false = (Mir.Literal (Float 0.), pos) in
  Bir.VariableSet.fold
    (fun var acc -> mk_or (mk_call_present var) acc)
    variables_to_check mk_false

let var_is_ (attr : string) (value : float) (v : Mir.Variable.t) : bool =
  List.exists
    (fun ((attr_name, _), (attr_value, _)) ->
      attr_name = attr && attr_value = Mast.Float value)
    v.Mir.Variable.attributes

let check_attribute (p : Mir_interface.full_program) (attr : string) : bool =
  Mir.CatVarMap.exists
    (fun _ Mir.{ attributs; _ } ->
      StrMap.exists (fun a _ -> a = attr) attributs)
    p.Mir_interface.program.Mir.program_var_categories

let cond_ExistsAttrWithVal (p : Mir_interface.full_program) (pos : Pos.t)
    ((attr, pos_attr) : string Pos.marked) (value : float) :
    Bir.expression Pos.marked =
  if check_attribute p attr then
    generate_input_condition (var_is_ attr value) p pos
  else Errors.raise_spanned_error "unknown attribute" pos_attr

let cond_ExistsAliases (p : Mir_interface.full_program) (pos : Pos.t)
    (aliases : Pos.t StrMap.t) : Bir.expression Pos.marked =
  let vars =
    StrMap.fold
      (fun var pos vmap ->
        Mir.VariableMap.add (Mir.find_var_by_name p.program (var, pos)) () vmap)
      aliases Mir.VariableMap.empty
  in
  generate_input_condition (fun v -> Mir.VariableMap.mem v vars) p pos

let wrap_m_code_call (m_program : Mir_interface.full_program)
    (order : Mir_interface.chain_order) (ctx : translation_ctx) :
    translation_ctx * Bir.stmt list =
  let m_program =
    {
      m_program with
      program =
        Bir_interpreter.FloatDefInterp.replace_undefined_with_input_variables
          m_program.program ctx.variables_used_as_inputs;
    }
  in
  let program_stmts =
    List.fold_left
      (fun stmts rov_id ->
        let rule = Mir.RuleMap.find rov_id m_program.program.program_rules in
        Pos.same_pos_as (Bir.SRovCall rov_id) rule.Mir.rule_number :: stmts)
      [] order.execution_order
  in
  let program_stmts = List.rev program_stmts in
  (ctx, program_stmts)

let generate_verif_cond (cond : Mir.condition_data) : Bir.stmt =
  let data = Mir.map_cond_data_var Bir.(var_from_mir default_tgv) cond in
  (Bir.SVerif data, Pos.get_position data.cond_expr)

let generate_verifs_prog (m_program : Mir_interface.full_program)
    (dom : Mast.DomainId.t) (expr : Mir.expression Pos.marked) =
  Mir_typechecker.typecheck_top_down ~in_generic_table:false expr;
  let my_floor a = floor (a +. 0.000001) in
  let _my_ceil a = ceil (a -. 0.000001) in
  let my_arr a =
    let my_var1 = floor a in
    let my_var2 = ((a -. my_var1) *. 100000.0) +. 0.5 in
    let my_var2 = floor my_var2 /. 100000.0 in
    let my_var2 = my_var1 +. my_var2 +. 0.5 in
    floor my_var2
  in
  let to_filter (expr : Mir.expression Pos.marked) cond =
    let rec aux env (expr : Mir.expression Pos.marked) =
      match Pos.unmark expr with
      | Mir.Literal l -> l
      | Mir.Unop (op, e0) -> begin
          match aux env e0 with
          | Mir.Undefined -> Mir.Undefined
          | Mir.Float f -> begin
              match op with
              | Mast.Not -> Mir.Float (1.0 -. f)
              | Mast.Minus -> Mir.Float ~-.f
            end
        end
      | Mir.FunctionCall (func, args) -> begin
          let rl = List.map (aux env) args in
          match func with
          | Mir.VerifNumber -> begin
              match Pos.unmark cond.Mir.cond_number with
              | Mir.VerifID id -> Mir.Float (float_of_int id)
              | _ -> assert false
            end
          | Mir.ComplNumber -> assert false
          | Mir.SumFunc ->
              List.fold_left
                (fun res r ->
                  match r with
                  | Mir.Undefined -> res
                  | Mir.Float f -> begin
                      match res with
                      | Mir.Undefined -> r
                      | Mir.Float fr -> Mir.Float (f +. fr)
                    end)
                Mir.Undefined rl
          | Mir.AbsFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (abs_float f)
              | _ -> assert false
            end
          | Mir.MinFunc -> begin
              match rl with
              | [ Mir.Undefined; Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Undefined; r ] | [ r; Mir.Undefined ] -> r
              | [ Mir.Float f0; Mir.Float f1 ] -> Mir.Float (min f0 f1)
              | _ -> assert false
            end
          | Mir.MaxFunc -> begin
              match rl with
              | [ Mir.Undefined; Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Undefined; r ] | [ r; Mir.Undefined ] -> r
              | [ Mir.Float f0; Mir.Float f1 ] -> Mir.Float (max f0 f1)
              | _ -> assert false
            end
          | Mir.GtzFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (if f > 0.0 then 1.0 else 0.0)
              | _ -> assert false
            end
          | Mir.GtezFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (if f >= 0.0 then 1.0 else 0.0)
              | _ -> assert false
            end
          | Mir.NullFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (if f = 0.0 then 1.0 else 0.0)
              | _ -> assert false
            end
          | Mir.ArrFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (my_arr f)
              | _ -> assert false
            end
          | Mir.InfFunc -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] -> Mir.Float (my_floor f)
              | _ -> assert false
            end
          | Mir.PresentFunc ->
              Errors.raise_spanned_error
                "function present is forbidden in verification filter"
                (Pos.get_position expr)
          | Mir.Multimax ->
              Errors.raise_spanned_error
                "function multimax is forbidden in verification filter"
                (Pos.get_position expr)
          | Mir.Supzero -> begin
              match rl with
              | [ Mir.Undefined ] -> Mir.Undefined
              | [ Mir.Float f ] when f = 0.0 -> Mir.Undefined
              | [ r ] -> r
              | _ -> assert false
            end
        end
      | Mir.Comparison (op, e0, e1) -> begin
          match (aux env e0, aux env e1) with
          | Mir.Undefined, _ | _, Mir.Undefined -> Mir.Undefined
          | Mir.Float f0, Mir.Float f1 -> begin
              match Pos.unmark op with
              | Mast.Gt -> Mir.Float (if f0 > f1 then 1.0 else 0.0)
              | Mast.Gte -> Mir.Float (if f0 >= f1 then 1.0 else 0.0)
              | Mast.Lt -> Mir.Float (if f0 < f1 then 1.0 else 0.0)
              | Mast.Lte -> Mir.Float (if f0 <= f1 then 1.0 else 0.0)
              | Mast.Eq -> Mir.Float (if f0 = f1 then 1.0 else 0.0)
              | Mast.Neq -> Mir.Float (if f0 <> f1 then 1.0 else 0.0)
            end
        end
      | Mir.Binop (op, e0, e1) -> begin
          let r0 = aux env e0 in
          let r1 = aux env e1 in
          match Pos.unmark op with
          | And -> begin
              match r0 with
              | Mir.Undefined -> Mir.Undefined
              | Mir.Float f0 -> if f0 = 0.0 then r0 else r1
            end
          | Or -> begin
              match r0 with
              | Mir.Undefined -> r1
              | Mir.Float f0 -> if f0 = 0.0 then r1 else r0
            end
          | Add -> begin
              match (r0, r1) with
              | Mir.Undefined, Mir.Undefined -> Mir.Undefined
              | Mir.Undefined, Mir.Float _ -> r1
              | Mir.Float _, Mir.Undefined -> r0
              | Mir.Float f0, Mir.Float f1 -> Mir.Float (f0 +. f1)
            end
          | Sub -> begin
              match (r0, r1) with
              | Mir.Undefined, Mir.Undefined -> Mir.Undefined
              | Mir.Undefined, Mir.Float _ -> r1
              | Mir.Float _, Mir.Undefined -> r0
              | Mir.Float f0, Mir.Float f1 -> Mir.Float (f0 +. f1)
            end
          | Mul -> begin
              match (r0, r1) with
              | Mir.Undefined, _ | _, Mir.Undefined -> Mir.Undefined
              | Mir.Float f0, Mir.Float f1 -> Mir.Float (f0 *. f1)
            end
          | Div -> begin
              match (r0, r1) with
              | Mir.Undefined, _ | _, Mir.Undefined -> Mir.Undefined
              | Mir.Float f0, Mir.Float f1 ->
                  if f1 = 0.0 then r1 else Mir.Float (f0 /. f1)
            end
        end
      | Mir.Conditional (e0, e1, e2) -> begin
          let r0 = aux env e0 in
          let r1 = aux env e1 in
          let r2 = aux env e2 in
          match r0 with
          | Mir.Undefined -> Mir.Undefined
          | Mir.Float f -> if f = 1.0 then r1 else r2
        end
      | Mir.Var v | Mir.Index ((v, _), _) ->
          Errors.raise_spanned_error
            "variables are forbidden in verification filter"
            (Pos.get_position v.Mir.name)
      | Mir.LocalVar lv -> begin
          match IntMap.find_opt lv.Mir.id env with
          | Some r -> r
          | None -> assert false
        end
      | Mir.LocalLet (lv, e0, e1) ->
          let r0 = aux env e0 in
          let env0 = IntMap.add lv.Mir.id r0 env in
          aux env0 e1
      | Mir.Error ->
          Errors.raise_spanned_error
            "errors are forbidden in verification filter"
            (Pos.get_position expr)
      | Mir.NbCategory cats ->
          let nb =
            Mir.fold_expr_var
              (fun res v ->
                match v.Mir.cats with
                | Some c when Mir.CatVarSet.mem c cats -> res +. 1.0
                | _ -> res)
              0.0
              (Pos.unmark cond.Mir.cond_expr)
          in
          Mir.Float nb
      | Mir.Attribut _ -> assert false
    in
    aux IntMap.empty expr
  in
  let is_verif_relevant rov_id cond =
    let is_verif = match rov_id with Mir.VerifID _ -> true | _ -> false in
    let verif_domain = cond.Mir.cond_domain in
    let is_max = Mast.DomainIdSet.mem dom verif_domain.dom_max in
    let is_eq = verif_domain.dom_id = dom in
    let is_var_compatible =
      (* !!! à valider en amont *)
      Mir.CatVarSet.subset
        (Mir.cond_cats_to_set cond.Mir.cond_cats)
        verif_domain.dom_data.vdom_auth
    in
    let is_kept = to_filter expr cond = Mir.Float 1.0 in
    is_verif && (is_max || is_eq) && is_var_compatible && is_kept
  in
  m_program.program.program_conds
  |> Mir.RuleMap.filter is_verif_relevant
  |> Mir.RuleMap.bindings
  |> List.sort (fun (_, cond1) (_, cond2) ->
         let res =
           Mast.compare_error_type (fst cond1.Mir.cond_error).typ
             (fst cond2.Mir.cond_error).typ
         in
         if res <> 0 then res
         else Stdlib.compare cond1.Mir.cond_seq_id cond2.Mir.cond_seq_id)
  |> List.map (fun (rov_id, cond) ->
         (Bir.SRovCall rov_id, Pos.get_position cond.Mir.cond_number))

let rec translate_m_code (m_program : Mir_interface.full_program)
    (ctx : translation_ctx) (instrs : Mir.instruction Pos.marked list) =
  let rec aux ctx res = function
    | [] -> (ctx, List.rev res)
    | (Mir.Affectation (vid, vdef), pos) :: instrs -> (
        try
          let var = Mir.VariableDict.find vid m_program.program.program_vars in
          let var_definition =
            Mir.map_var_def_var
              Bir.(var_from_mir default_tgv)
              vdef.Mir.var_definition
          in
          match var_definition with
          | InputVar -> aux ctx res instrs
          | TableVar _ | SimpleVar _ ->
              aux ctx
                (( Bir.SAssign
                     (Bir.(var_from_mir default_tgv) var, var_definition),
                   var.Mir.Variable.execution_number.pos )
                :: res)
                instrs
        with Not_found ->
          Errors.raise_spanned_error
            (Format.sprintf "unknown variable id %d" vid)
            pos)
    | (Mir.IfThenElse (e, ilt, ile), pos) :: instrs ->
        let expr = Mir.map_expr_var Bir.(var_from_mir default_tgv) e in
        let ctx, stmts_then = translate_m_code m_program ctx ilt in
        let ctx, stmts_else = translate_m_code m_program ctx ile in
        aux ctx
          ((Bir.SConditional (expr, stmts_then, stmts_else), pos) :: res)
          instrs
    | (Mir.ComputeDomain l, _pos) :: instrs ->
        let dom = Mast.DomainId.from_marked_list (Pos.unmark l) in
        let order =
          match Mast.DomainIdMap.find_opt dom m_program.domains_orders with
          | Some order -> order
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf "Unknown rule domain: %a" (Mast.DomainId.pp ())
                   dom)
                (Pos.get_position l)
        in
        let ctx =
          {
            ctx with
            used_rule_domains = Mast.DomainIdSet.add dom ctx.used_rule_domains;
          }
        in
        let ctx, stmts = wrap_m_code_call m_program order ctx in
        aux ctx (List.rev stmts @ res) instrs
    | (Mir.ComputeChaining ch, _pos) :: instrs ->
        let chain = Pos.unmark ch in
        let order =
          match Mast.ChainingMap.find_opt chain m_program.chainings_orders with
          | Some order -> order
          | None ->
              Errors.raise_spanned_error
                (Format.sprintf "Unknown chaining: %s" chain)
                (Pos.get_position ch)
        in
        let ctx =
          {
            ctx with
            used_chainings = Mast.ChainingSet.add chain ctx.used_chainings;
          }
        in
        let ctx, stmts = wrap_m_code_call m_program order ctx in
        aux ctx (List.rev stmts @ res) instrs
    | (Mir.ComputeTarget tn, pos) :: instrs ->
        let name = Pos.unmark tn in
        let ctx, stmt =
          match
            Mir.TargetMap.find_opt name m_program.program.program_targets
          with
          | Some _ -> (ctx, (Bir.SFunctionCall (name, []), pos))
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf "Unknown target: %s" name)
                (Pos.get_position tn)
        in
        aux ctx (stmt :: res) instrs
    | (Mir.ComputeVerifs (l, expr), _pos) :: instrs ->
        let dom = Mast.DomainId.from_marked_list (Pos.unmark l) in
        let ctx = { ctx with verif_seen = true } in
        let stmts = generate_verifs_prog m_program dom expr in
        aux ctx (List.rev stmts @ res) instrs
    | (Mir.Print (std, args), pos) :: instrs ->
        let bir_args =
          List.rev
            (List.fold_left
               (fun res arg ->
                 let bir_arg =
                   match Pos.unmark arg with
                   | Mir.PrintString s -> Mir.PrintString s
                   | Mir.PrintName (v, vid) -> Mir.PrintName (v, vid)
                   | Mir.PrintAlias (v, vid) -> Mir.PrintAlias (v, vid)
                   | Mir.PrintExpr (e, min, max) ->
                       Mir.PrintExpr
                         ( Pos.same_pos_as
                             (Mir.map_expr_var
                                Bir.(var_from_mir default_tgv)
                                (Pos.unmark e))
                             e,
                           min,
                           max )
                 in
                 bir_arg :: res)
               [] args)
        in
        aux ctx ((Bir.SPrint (std, bir_args), pos) :: res) instrs
    | (Mir.Iterate (v, vcs, e, iit), pos) :: instrs ->
        let var =
          Bir.(var_from_mir default_tgv)
            (Mir.VariableDict.find v m_program.program.program_vars)
        in
        let expr =
          Mir.map_expr_var Bir.(var_from_mir default_tgv) (Pos.unmark e)
        in
        let ctx, stmts = translate_m_code m_program ctx iit in
        aux ctx ((Bir.SIterate (var, vcs, expr, stmts), pos) :: res) instrs
    | (Mir.Restore (vars, var_params, irest), pos) :: instrs ->
        let vars =
          Mir.VariableMap.fold
            (fun v _ vars ->
              let var =
                Bir.(var_from_mir default_tgv)
                  (Mir.VariableDict.find v.Mir.id m_program.program.program_vars)
              in
              Bir.VariableSet.add var vars)
            vars Bir.VariableSet.empty
        in
        let var_params =
          List.fold_left
            (fun var_params ((v : Mir.variable), vcs, expr) ->
              let var =
                Bir.(var_from_mir default_tgv)
                  (Mir.VariableDict.find v.Mir.id m_program.program.program_vars)
              in
              let expr =
                Mir.map_expr_var
                  Bir.(var_from_mir default_tgv)
                  (Pos.unmark expr)
              in
              (var, vcs, expr) :: var_params)
            [] var_params
        in
        let ctx, stmts = translate_m_code m_program ctx irest in
        aux ctx ((Bir.SRestore (vars, var_params, stmts), pos) :: res) instrs
  in
  aux ctx [] instrs

type filter_val = Int of int | Bool of bool

let generate_verif_call (m_program : Mir_interface.full_program)
    (chain : Mast.DomainId.t) (filter : Mpp_ir.mpp_expr) : Bir.stmt list =
  let rec to_filter expr cond =
    match Pos.unmark expr with
    | Mpp_ir.Constant i -> Int i
    | Mpp_ir.Variable _ ->
        Errors.raise_spanned_error "forbidden subexpression"
          (Pos.get_position expr)
    | Mpp_ir.Unop (Mpp_ir.Minus, e) -> begin
        match to_filter e cond with
        | Int i -> Int (-i)
        | Bool _ ->
            Errors.raise_spanned_error "integer expression expected"
              (Pos.get_position e)
      end
    | Mpp_ir.Call (Mpp_ir.NbVarCat cvs, _) ->
        let i =
          Mir.CatVarSet.fold
            (fun cv res ->
              match Mir.CatVarMap.find_opt cv cond.Mir.cond_cats with
              | Some i -> i + res
              | None ->
                  Errors.raise_spanned_error "unknown variable category"
                    (Pos.get_position expr))
            cvs 0
        in
        Int i
    | Mpp_ir.Call (_, _) ->
        Errors.raise_spanned_error "forbidden function" (Pos.get_position expr)
    | Mpp_ir.Binop (e1, b, e2) -> begin
        let r1 = to_filter e1 cond in
        let r2 = to_filter e2 cond in
        match (r1, b, r2) with
        | Bool b1, Mpp_ast.And, Bool b2 -> Bool (b1 && b2)
        | Bool b1, Mpp_ast.Or, Bool b2 -> Bool (b1 || b2)
        | Int i1, Mpp_ast.Gt, Int i2 -> Bool (i1 > i2)
        | Int i1, Mpp_ast.Gte, Int i2 -> Bool (i1 >= i2)
        | Int i1, Mpp_ast.Lt, Int i2 -> Bool (i1 < i2)
        | Int i1, Mpp_ast.Lte, Int i2 -> Bool (i1 <= i2)
        | Int i1, Mpp_ast.Eq, Int i2 -> Bool (i1 = i2)
        | Int i1, Mpp_ast.Neq, Int i2 -> Bool (i1 <> i2)
        | Int _, Mpp_ast.(And | Or), _ ->
            Errors.raise_spanned_error "boolean expression expected"
              (Pos.get_position e1)
        | _, Mpp_ast.(And | Or), Int _ ->
            Errors.raise_spanned_error "boolean expression expected"
              (Pos.get_position e2)
        | Bool _, Mpp_ast.(Gt | Gte | Lt | Lte | Eq | Neq), _ ->
            Errors.raise_spanned_error "integer expression expected"
              (Pos.get_position e1)
        | _, Mpp_ast.(Gt | Gte | Lt | Lte | Eq | Neq), Bool _ ->
            Errors.raise_spanned_error "integer expression expected"
              (Pos.get_position e2)
      end
  in
  let is_verif_relevant _ cond =
    (* specific restriction *)
    let cats = Mir.cond_cats_to_set cond.Mir.cond_cats in
    let verif_domain = cond.Mir.cond_domain in
    let is_max = Mast.DomainIdSet.mem chain verif_domain.dom_max in
    let is_eq = verif_domain.dom_id = chain in
    let is_var_compatible =
      Mir.CatVarSet.subset cats verif_domain.dom_data.vdom_auth
    in
    let is_kept = to_filter filter cond = Bool true in
    (is_max || is_eq) && is_var_compatible && is_kept
  in
  let relevant_verifs =
    Mir.RuleMap.filter is_verif_relevant m_program.program.program_conds
  in
  let verifs =
    Mir.RuleMap.bindings relevant_verifs
    |> List.sort (fun (_, cond1) (_, cond2) ->
           let res =
             Mast.compare_error_type (fst cond1.Mir.cond_error).typ
               (fst cond2.Mir.cond_error).typ
           in
           if res <> 0 then res
           else Stdlib.compare cond1.Mir.cond_seq_id cond2.Mir.cond_seq_id)
    |> List.map snd
  in
  List.map
    (fun verif ->
      Pos.map_under_mark
        (fun verif_id -> Bir.SRovCall verif_id)
        verif.Mir.cond_number)
    verifs

let rec translate_mpp_function (mpp_program : Mpp_ir.mpp_compute list)
    (m_program : Mir_interface.full_program) (compute_decl : Mpp_ir.mpp_compute)
    (args : Mpp_ir.scoped_var list) (ctx : translation_ctx) :
    translation_ctx * Bir.stmt list =
  translate_mpp_stmts mpp_program m_program args ctx compute_decl.Mpp_ir.body

and translate_mpp_expr (p : Mir_interface.full_program) (ctx : translation_ctx)
    (expr : Mpp_ir.mpp_expr_kind Pos.marked) : Bir.expression =
  let pos = Pos.get_position expr in
  match Pos.unmark expr with
  | Mpp_ir.Constant i -> Mir.Literal (Float (float_of_int i))
  | Variable (Mbased (var, _)) -> Var Bir.(var_from_mir default_tgv var)
  | Variable (Local l) -> (
      try Var (StrMap.find l ctx.new_variables)
      with Not_found ->
        Cli.error_print "Local Variable %s not found in ctx" l;
        assert false)
  | Unop (Minus, e) -> Mir.Unop (Mast.Minus, (translate_mpp_expr p ctx e, pos))
  | Binop (e1, ((And | Or) as b), e2) ->
      Mir.Binop
        ( (translate_to_binop b, pos),
          (translate_mpp_expr p ctx e1, pos),
          (translate_mpp_expr p ctx e2, pos) )
  | Binop (e1, b, e2) ->
      Mir.Comparison
        ( (translate_to_compop b, pos),
          (translate_mpp_expr p ctx e1, pos),
          (translate_mpp_expr p ctx e2, pos) )
  | Call (Present, [ l ]) ->
      Pos.unmark @@ Mir_typechecker.expand_functions_expr
      @@ ( Mir.FunctionCall
             ( PresentFunc,
               [ (translate_mpp_expr p ctx (Mpp_ir.Variable l, pos), pos) ] ),
           pos )
  | Call (Abs, [ l ]) ->
      Pos.unmark @@ Mir_typechecker.expand_functions_expr
      @@ ( Mir.FunctionCall
             ( AbsFunc,
               [ (translate_mpp_expr p ctx (Mpp_ir.Variable l, pos), pos) ] ),
           pos )
  | Call (Cast, [ l ]) ->
      Mir.Binop
        ( (Mast.Add, pos),
          (translate_mpp_expr p ctx (Mpp_ir.Variable l, pos), pos),
          (Mir.Literal (Float 0.), pos) )
  | Call (ExistsAttrWithVal (attr, value), []) ->
      Pos.unmark @@ cond_ExistsAttrWithVal p pos attr value
  | Call (ExistsAliases aliases, []) ->
      Pos.unmark @@ cond_ExistsAliases p pos aliases
  | Call (NbVarCat _, []) ->
      Errors.raise_spanned_error "forbidden expression" pos
  | _ -> assert false

and translate_mpp_stmt (mpp_program : Mpp_ir.mpp_compute list)
    (m_program : Mir_interface.full_program) func_args (ctx : translation_ctx)
    (stmt : Mpp_ir.mpp_stmt) : translation_ctx * Bir.stmt list =
  let pos = Pos.get_position stmt in
  match Pos.unmark stmt with
  | Mpp_ir.Assign (Local l, expr) ->
      let ctx, new_l =
        match StrMap.find_opt l ctx.new_variables with
        | None ->
            let new_l =
              Mir.Variable.new_var
                ("mpp_" ^ l, pos)
                None ("", pos)
                (Mast_to_mir.dummy_exec_number pos)
                ~attributes:[] ~origin:None ~cats:None ~is_table:None
                ~is_temp:false ~is_it:false
              |> Bir.(var_from_mir default_tgv)
            in
            let ctx =
              { ctx with new_variables = StrMap.add l new_l ctx.new_variables }
            in
            (ctx, new_l)
        | Some new_l -> (ctx, new_l)
      in
      ( ctx,
        [
          Pos.same_pos_as
            (Bir.SAssign
               (new_l, SimpleVar (translate_mpp_expr m_program ctx expr, pos)))
            stmt;
        ] )
  | Mpp_ir.Assign (Mbased (var, _), expr) ->
      ( {
          ctx with
          variables_used_as_inputs =
            Mir.VariableDict.add var ctx.variables_used_as_inputs;
        },
        [
          Pos.same_pos_as
            (Bir.SAssign
               ( Bir.(var_from_mir default_tgv) var,
                 SimpleVar (translate_mpp_expr m_program ctx expr, pos) ))
            stmt;
        ] )
  | Mpp_ir.Conditional (e, t, f) ->
      let e' = translate_mpp_expr m_program ctx e in
      let ctx1, rt' =
        translate_mpp_stmts mpp_program m_program func_args ctx t
      in
      let ctx2, rf' =
        translate_mpp_stmts mpp_program m_program func_args ctx f
      in
      ( ctx_join ctx1 ctx2,
        [ Pos.same_pos_as (Bir.SConditional (e', rt', rf')) stmt ] )
  | Mpp_ir.Delete (Mbased (var, _)) ->
      ( ctx,
        [
          Pos.same_pos_as
            (Bir.SAssign
               ( Bir.(var_from_mir default_tgv) var,
                 SimpleVar (Mir.Literal Undefined, pos) ))
            stmt;
        ] )
  | Mpp_ir.Delete (Local l) ->
      let var = StrMap.find l ctx.new_variables in
      ( ctx,
        [
          Pos.same_pos_as
            (Bir.SAssign (var, SimpleVar (Mir.Literal Undefined, pos)))
            stmt;
        ] )
  | Mpp_ir.Expr (Call (MppFunction f, args), pos) ->
      let real_args =
        match args with [ Mpp_ir.Local "outputs" ] -> func_args | _ -> args
      in
      ( ctx,
        [
          ( Bir.SFunctionCall
              ( f,
                List.map
                  (function
                    | Mpp_ir.Local _ -> assert false
                    | Mpp_ir.Mbased (var, _) -> var)
                  real_args ),
            pos );
        ] )
  | Mpp_ir.Expr (Call (Target t, _args), pos) -> begin
      match Mir.TargetMap.find_opt t m_program.program.program_targets with
      | Some _ -> (ctx, [ (Bir.SFunctionCall (t, []), pos) ])
      | None ->
          Errors.raise_spanned_error
            (Format.asprintf "Unknown target: %s" t)
            pos
    end
  | Mpp_ir.Expr (Call (Rules dom, _args), _) ->
      let order =
        match Mast.DomainIdMap.find_opt dom m_program.domains_orders with
        | Some order -> order
        | None ->
            Errors.raise_error
              (Format.asprintf "Unknown rule domain: %a" (Mast.DomainId.pp ())
                 dom)
      in
      let ctx =
        {
          ctx with
          used_rule_domains = Mast.DomainIdSet.add dom ctx.used_rule_domains;
        }
      in
      wrap_m_code_call m_program order ctx
  | Mpp_ir.Expr (Call (Chain chain, _args), _) ->
      let order =
        match Mast.ChainingMap.find_opt chain m_program.chainings_orders with
        | Some order -> order
        | None ->
            Errors.raise_error (Format.sprintf "Unknown chaining: %s" chain)
      in
      let ctx =
        {
          ctx with
          used_chainings = Mast.ChainingSet.add chain ctx.used_chainings;
        }
      in
      wrap_m_code_call m_program order ctx
  | Mpp_ir.Expr (Call (Verifs (dom, filter), _args), _) ->
      ({ ctx with verif_seen = true }, generate_verif_call m_program dom filter)
  | Mpp_ir.Partition ((attr, pos_attr), value, body) ->
      if not (check_attribute m_program attr) then
        Errors.raise_spanned_error "unknown attribute" pos_attr;
      let func_of_filter = var_is_ attr value in
      let ctx, partition_pre, partition_post =
        generate_partition mpp_program m_program func_args func_of_filter pos
          ctx
      in
      let ctx, body =
        translate_mpp_stmts mpp_program m_program func_args ctx body
      in
      (ctx, partition_pre @ body @ partition_post)
  | _ -> assert false

and translate_mpp_stmts (mpp_program : Mpp_ir.mpp_compute list)
    (m_program : Mir_interface.full_program)
    (func_args : Mpp_ir.scoped_var list) (ctx : translation_ctx)
    (stmts : Mpp_ir.mpp_stmt list) : translation_ctx * Bir.stmt list =
  List.fold_left
    (fun (ctx, stmts) stmt ->
      let ctx, stmt =
        translate_mpp_stmt mpp_program m_program func_args ctx stmt
      in
      (ctx, stmts @ stmt))
    (ctx, []) stmts

and generate_partition mpp_program m_program func_args
    (filter : Mir.Variable.t -> bool) (pos : Pos.t) (ctx : translation_ctx) :
    translation_ctx * Bir.stmt list * Bir.stmt list =
  let vars_to_move =
    Mir.VariableDict.fold
      (fun var acc -> if filter var then var :: acc else acc)
      m_program.program.program_vars []
  in
  let mpp_pre, mpp_post =
    List.fold_left
      (fun (stmts_pre, stmts_post) var ->
        let shadow_var_name = "_" ^ Pos.unmark var.Mir.Variable.name in
        let open Mpp_ir in
        let shadow_var = Local shadow_var_name in
        let var = Mbased (var, Output) in
        ( (Assign (shadow_var, (Variable var, pos)), pos)
          :: (Delete var, pos) :: stmts_pre,
          (Assign (var, (Variable shadow_var, pos)), pos)
          :: (Delete shadow_var, pos) :: stmts_post ))
      ([], []) vars_to_move
  in
  let ctx, pre =
    translate_mpp_stmts mpp_program m_program func_args ctx mpp_pre
  in
  let ctx, post =
    translate_mpp_stmts mpp_program m_program func_args ctx mpp_post
  in
  (ctx, pre, post)

let create_combined_program (m_program : Mir_interface.full_program)
    (mpp_program : Mpp_ir.mpp_program) (mpp_function_to_extract : string) :
    Bir.program =
  try
    let mpp_program = List.rev mpp_program in
    let ctx, mpp_functions =
      List.fold_left
        (fun (ctx, function_map) mpp_func ->
          let ctx, mppf_stmts =
            translate_mpp_function mpp_program m_program mpp_func [] ctx
          in
          let func = Bir.{ mppf_stmts; mppf_is_verif = ctx.verif_seen } in
          let ctx = { ctx with verif_seen = false } in
          (ctx, Bir.FunctionMap.add mpp_func.name func function_map))
        (empty_translation_ctx, Bir.FunctionMap.empty)
        mpp_program
    in
    let ctx, targets =
      Mir.TargetMap.fold
        (fun n t (ctx, targets) ->
          let ctx, code = translate_m_code m_program ctx t.Mir.target_prog in
          ( ctx,
            Mir.TargetMap.add n
              Bir.
                {
                  tmp_vars =
                    StrMap.map
                      (fun (var, pos, size) ->
                        (Bir.(var_from_mir default_tgv) var, pos, size))
                      t.Mir.target_tmp_vars;
                  stmts = code;
                  is_verif = ctx.verif_seen;
                }
              targets ))
        m_program.program.program_targets (ctx, Mir.TargetMap.empty)
    in
    let rules =
      Mir.RuleMap.fold
        (fun rov_id rule_data rules ->
          if
            let rule_domain = rule_data.Mir.rule_domain in
            let has_max =
              not
                (Mast.DomainIdSet.disjoint ctx.used_rule_domains
                   rule_domain.dom_max)
            in
            let has_used_domain =
              Mast.DomainIdSet.mem rule_domain.dom_id ctx.used_rule_domains
            in
            let has_used_chaining =
              match rule_data.Mir.rule_chain with
              | None -> false
              | Some (ch, _) -> Mast.ChainingSet.mem ch ctx.used_chainings
            in
            let is_not_rule_0 =
              Pos.unmark rule_data.Mir.rule_number <> RuleID 0
            in
            is_not_rule_0 && (has_max || has_used_domain || has_used_chaining)
          then
            let rov_name =
              Pos.map_under_mark
                (fun n -> string_of_int (Mir.num_of_rule_or_verif_id n))
                rule_data.Mir.rule_number
            in
            let rov_code =
              Bir.Rule
                (snd (translate_m_code m_program ctx rule_data.Mir.rule_vars))
            in
            Mir.RuleMap.add rov_id Bir.{ rov_id; rov_name; rov_code } rules
          else rules)
        m_program.program.program_rules Mir.RuleMap.empty
    in
    let rules_and_verifs =
      Mir.RuleMap.fold
        (fun _ cond_data rules ->
          let rov_id = Pos.unmark cond_data.Mir.cond_number in
          let rov_name =
            Pos.same_pos_as
              (string_of_int (Mir.num_of_rule_or_verif_id rov_id))
              cond_data.Mir.cond_number
          in
          let rov_code = Bir.Verif (generate_verif_cond cond_data) in
          Mir.RuleMap.add rov_id Bir.{ rov_id; rov_name; rov_code } rules)
        m_program.program.program_conds rules
    in
    if
      (not (Bir.FunctionMap.mem mpp_function_to_extract mpp_functions))
      && not (Mir.TargetMap.mem mpp_function_to_extract targets)
    then
      Errors.raise_error
        (Format.asprintf "M++ function %s not found in M++ file!"
           mpp_function_to_extract);
    {
      targets;
      rules_and_verifs;
      mpp_functions;
      main_function = mpp_function_to_extract;
      context = None;
      idmap = m_program.program.program_idmap;
      mir_program = m_program.program;
      outputs = Bir.VariableMap.empty;
    }
  with Bir_interpreter.FloatDefInterp.RuntimeError (r, ctx) ->
    Bir_interpreter.FloatDefInterp.raise_runtime_as_structured r ctx
      m_program.program
