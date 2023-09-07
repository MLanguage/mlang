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

let rec list_map_opt (f : 'a -> 'b option) (l : 'a list) : 'b list =
  match l with
  | [] -> []
  | hd :: tl -> (
      match f hd with
      | None -> list_map_opt f tl
      | Some fhd -> fhd :: list_map_opt f tl)

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
    (fun _ (attrs, _) -> StrMap.exists (fun a _ -> a = attr) attrs)
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

let rec translate_m_code (m_program : Mir_interface.full_program)
    (instrs : Mir.instruction Pos.marked list) =
  list_map_opt
    (function
      | Mir.Affectation (vid, vdef), pos -> (
          try
            let var =
              Mir.VariableDict.find vid m_program.program.program_vars
            in
            let var_definition =
              Mir.map_var_def_var
                Bir.(var_from_mir default_tgv)
                vdef.Mir.var_definition
            in
            match var_definition with
            | InputVar -> None
            | TableVar _ | SimpleVar _ ->
                Some
                  ( Bir.SAssign
                      (Bir.(var_from_mir default_tgv) var, var_definition),
                    var.Mir.Variable.execution_number.pos )
          with Not_found ->
            Errors.raise_spanned_error
              (Format.sprintf "unknown variable id %d" vid)
              pos)
      | Mir.IfThenElse (e, ilt, ile), pos ->
          let expr = Mir.map_expr_var Bir.(var_from_mir default_tgv) e in
          let stmts_then = translate_m_code m_program ilt in
          let stmts_else = translate_m_code m_program ile in
          Some (Bir.SConditional (expr, stmts_then, stmts_else), pos))
    instrs

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
                ~attributes:[] ~origin:None ~cats:Mir.CatVarSet.empty
                ~is_table:None ~is_temp:false
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
              Bir.Rule (translate_m_code m_program rule_data.Mir.rule_vars)
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
    let targets =
      Mir.TargetMap.map
        (fun t -> translate_m_code m_program t.Mir.target_prog)
        m_program.program.program_targets
    in
    if not (Bir.FunctionMap.mem mpp_function_to_extract mpp_functions) then
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
