(* Copyright (C) 2019-2021 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

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

module D = DecoupledExpr

let str_escape str =
  let l = String.length str in
  let buf = Buffer.create l in
  let rec aux = function
    | i when i >= l -> Buffer.contents buf
    | i -> begin
        let c = str.[i] in
        let ic = Char.code c in
        match c with
        | '"' | '%' ->
            let cc = Format.sprintf "\\%03o" ic in
            Buffer.add_string buf cc;
            aux (i + 1)
        | _c when ic <= 31 || ic >= 127 ->
            let cc = Format.sprintf "\\%03o" ic in
            Buffer.add_string buf cc;
            aux (i + 1)
        | c ->
            Buffer.add_char buf c;
            aux (i + 1)
      end
  in
  aux 0

let fresh_c_local =
  let c = ref 0 in
  fun name ->
    let s = name ^ string_of_int !c in
    incr c;
    s

let rec generate_c_expr (e : Mir.expression Pos.marked)
    (var_indexes : Dgfip_varid.var_id_map) : D.expression_composition =
  let comparison op se1 se2 =
    let safe_def = false in
    let def_test = D.dand se1.D.def_test se2.D.def_test in
    let value_comp =
      let op =
        let open Com in
        match Pos.unmark op with
        | Gt -> ">"
        | Gte -> ">="
        | Lt -> "<"
        | Lte -> "<="
        | Eq -> "=="
        | Neq -> "!="
      in
      D.comp op se1.value_comp se2.value_comp
    in
    D.build_transitive_composition ~safe_def { def_test; value_comp }
  in
  let binop op se1 se2 =
    match op with
    | Com.Div, _ ->
        let def_test = D.dand se1.D.def_test se2.D.def_test in
        let value_comp =
          D.ite se2.value_comp (D.div se1.value_comp se2.value_comp) (D.lit 0.)
        in
        D.build_transitive_composition ~safe_def:true { def_test; value_comp }
    | _ ->
        let def_test =
          match Pos.unmark op with
          | Com.And | Com.Mul -> D.dand se1.def_test se2.def_test
          | Com.Or | Com.Add | Com.Sub -> D.dor se1.def_test se2.def_test
          | Com.Div -> assert false
          (* see above *)
        in
        let op e1 e2 =
          match Pos.unmark op with
          | Com.And -> D.dand e1 e2
          | Com.Or -> D.dor e1 e2
          | Com.Add -> D.plus e1 e2
          | Com.Sub -> D.sub e1 e2
          | Com.Mul -> D.mult e1 e2
          | Com.Div -> assert false
          (* see above *)
        in
        let value_comp = op se1.value_comp se2.value_comp in
        D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  in
  let unop op se =
    let def_test = se.D.def_test in
    let op, safe_def =
      match op with Com.Not -> (D.dnot, false) | Com.Minus -> (D.minus, true)
    in
    let value_comp = op se.value_comp in
    D.build_transitive_composition ~safe_def { def_test; value_comp }
  in
  match Pos.unmark e with
  | Com.TestInSet (positive, e0, values) ->
      let se0 = generate_c_expr e0 var_indexes in
      let ldef, lval = D.locals_from_m (Mir.LocalVariable.new_var ()) in
      let sle0 = D.{ def_test = local_var ldef; value_comp = local_var lval } in
      let declare_local constr =
        D.let_local ldef se0.def_test (D.let_local lval se0.value_comp constr)
      in
      let or_chain =
        List.fold_left
          (fun or_chain set_value ->
            let equal_test =
              match set_value with
              | Com.VarValue set_var ->
                  let s_set_var =
                    let v = Pos.unmark set_var in
                    let def_test = D.m_var v None Def in
                    let value_comp = D.m_var v None Val in
                    D.{ def_test; value_comp }
                  in
                  comparison (Com.Eq, Pos.no_pos) sle0 s_set_var
              | Com.FloatValue i ->
                  let s_i =
                    D.{ def_test = dtrue; value_comp = lit (Pos.unmark i) }
                  in
                  comparison (Com.Eq, Pos.no_pos) sle0 s_i
              | Com.Interval (bn, en) ->
                  let s_bn =
                    let bn' = float_of_int (Pos.unmark bn) in
                    D.{ def_test = dtrue; value_comp = lit bn' }
                  in
                  let s_en =
                    let en' = float_of_int (Pos.unmark en) in
                    D.{ def_test = dtrue; value_comp = lit en' }
                  in
                  binop (Com.And, Pos.no_pos)
                    (comparison (Com.Gte, Pos.no_pos) sle0 s_bn)
                    (comparison (Com.Lte, Pos.no_pos) sle0 s_en)
            in
            binop (Com.Or, Pos.no_pos) or_chain equal_test)
          D.{ def_test = dfalse; value_comp = lit 0. }
          values
      in
      let se = if positive then or_chain else unop Com.Not or_chain in
      D.
        {
          def_test = declare_local se.def_test;
          value_comp = declare_local se.value_comp;
        }
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      comparison op se1 se2
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      binop op se1 se2
  | Unop (op, e) ->
      let se = generate_c_expr e var_indexes in
      unop op se
  | Index (var, e) ->
      let idx = generate_c_expr e var_indexes in
      let size = Dgfip_varid.gen_size var_indexes (Pos.unmark var) in
      let idx_var = D.new_local () in
      let def_test =
        D.let_local idx_var idx.value_comp
          (D.dand
             (D.dand idx.def_test
                (D.comp "<" (D.local_var idx_var) (D.dinstr size)))
             (D.access (Pos.unmark var) Def (D.local_var idx_var)))
      in
      let value_comp =
        D.let_local idx_var idx.value_comp
          (D.ite
             (D.comp "<" (D.local_var idx_var) (D.lit 0.))
             (D.lit 0.)
             (D.access (Pos.unmark var) Val (D.local_var idx_var)))
      in
      D.build_transitive_composition { def_test; value_comp }
  | Conditional (c, t, f_opt) ->
      let cond = generate_c_expr c var_indexes in
      let thenval = generate_c_expr t var_indexes in
      let elseval =
        match f_opt with
        | None -> D.{ def_test = dfalse; value_comp = lit 0. }
        | Some f -> generate_c_expr f var_indexes
      in
      let def_test =
        D.dand cond.def_test
          (D.ite cond.value_comp thenval.def_test elseval.def_test)
      in
      let value_comp =
        D.ite cond.value_comp thenval.value_comp elseval.value_comp
      in
      D.build_transitive_composition { def_test; value_comp }
  | FuncCall ((Supzero, _), [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let cond = D.dand se.def_test (D.comp ">=" se.value_comp (D.lit 0.0)) in
      let def_test = D.ite cond D.dfalse se.def_test in
      let value_comp = D.ite cond (D.lit 0.0) se.value_comp in
      D.build_transitive_composition { def_test; value_comp }
  | FuncCall ((PresentFunc, _), [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = D.dtrue in
      let value_comp = se.def_test in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((NullFunc, _), [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp =
        D.dand def_test (D.comp "==" se.value_comp (D.lit 0.0))
      in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((ArrFunc, _), [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_arr" [ se.value_comp ] in
      (* Here we boldly assume that rounding value of `undef` will give zero,
         given the invariant. Pretty sure that not true, in case of doubt, turn
         `safe_def` to false *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((InfFunc, _), [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_floor" [ se.value_comp ] in
      (* same as above *)
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((AbsFunc, _), [ arg ]) ->
      let se = generate_c_expr arg var_indexes in
      let def_test = se.def_test in
      let value_comp = D.dfun "fabs" [ se.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((MaxFunc, _), [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "max" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((MinFunc, _), [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 var_indexes in
      let se2 = generate_c_expr e2 var_indexes in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "min" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true { def_test; value_comp }
  | FuncCall ((Multimax, _), [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr e1 var_indexes in
      let def_test =
        D.dfun "multimax_def" [ bound.value_comp; D.m_var v2 PassPointer Def ]
      in
      let value_comp =
        D.dfun "multimax" [ bound.value_comp; D.m_var v2 PassPointer Val ]
      in
      D.build_transitive_composition { def_test; value_comp }
  | FuncCall _ -> assert false (* should not happen *)
  | Literal (Float f) -> { def_test = D.dtrue; value_comp = D.lit f }
  | Literal Undefined -> { def_test = D.dfalse; value_comp = D.lit 0. }
  | Var var ->
      { def_test = D.m_var var None Def; value_comp = D.m_var var None Val }
  | Attribut (var, a) ->
      let ptr, var_cat_data =
        match Mir.VariableMap.find (Pos.unmark var) var_indexes with
        | Dgfip_varid.VarIterate (t, _, vcd) -> (t, vcd)
        | _ -> assert false
      in
      let id_str = var_cat_data.id_str in
      let def_test =
        D.dinstr
          (Format.sprintf "attribut_%s_def(%s, \"%s\")" id_str ptr
             (Pos.unmark a))
      in
      let value_comp =
        D.dinstr
          (Format.sprintf "attribut_%s(%s, \"%s\")" id_str ptr (Pos.unmark a))
      in
      D.build_transitive_composition { def_test; value_comp }
  | Size var ->
      let ptr =
        match Mir.VariableMap.find (Pos.unmark var) var_indexes with
        | Dgfip_varid.VarIterate (t, _, _) -> t
        | _ -> assert false
      in
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr (Format.sprintf "(%s->size)" ptr) in
      D.build_transitive_composition { def_test; value_comp }
  | NbAnomalies ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_anomalies(irdata)" in
      D.build_transitive_composition { def_test; value_comp }
  | NbDiscordances ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_discordances(irdata)" in
      D.build_transitive_composition { def_test; value_comp }
  | NbInformatives ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_informatives(irdata)" in
      D.build_transitive_composition { def_test; value_comp }
  | NbBloquantes ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_bloquantes(irdata)" in
      D.build_transitive_composition { def_test; value_comp }
  | NbCategory _ -> assert false
  | FuncCallLoop _ | Loop _ -> assert false

let generate_m_assign (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : Mir.Var.t) (offset : D.offset)
    (oc : Format.formatter) (se : D.expression_composition) : unit =
  let def_var = D.generate_variable ~def_flag:true var_indexes offset var in
  let val_var = D.generate_variable var_indexes offset var in
  let locals, def, value = D.build_expression se in
  if D.is_always_true def then
    Format.fprintf oc "%a%a@,@[<v 2>{@,%a@,@]}" D.format_local_declarations
      locals
      (D.format_assign dgfip_flags var_indexes def_var)
      def
      (D.format_assign dgfip_flags var_indexes val_var)
      value
  else
    Format.fprintf oc "%a%a@,@[<v 2>if(%s){@;%a@]@,}@,else %s = 0.;"
      D.format_local_declarations locals
      (D.format_assign dgfip_flags var_indexes def_var)
      def def_var
      (D.format_assign dgfip_flags var_indexes val_var)
      value val_var;
  (* If the trace flag is set, we print the value of all non-temp variables *)
  if dgfip_flags.flg_trace && not var.Mir.Variable.is_temp then
    Format.fprintf oc "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Mir.Var.name)
      (Dgfip_varid.gen_access_pos_from_start var_indexes var)

let generate_var_def (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : Mir.Var.t)
    (vidx_opt : Mir.expression Pos.marked option)
    (vexpr : Mir.expression Pos.marked) (fmt : Format.formatter) : unit =
  match vidx_opt with
  | None ->
      let se = generate_c_expr vexpr var_indexes in
      if Mir.Var.is_it var then (
        let pr form = Format.fprintf fmt form in
        pr "@[<v 2>{";
        let idx = fresh_c_local "idxPROUT" in
        pr "@;int %s;" idx;
        pr "@;@[<v 2>for(%s = 0; %s < %s; %s++) {" idx idx
          (Dgfip_varid.gen_size var_indexes var)
          idx;
        pr "@;%a"
          (generate_m_assign dgfip_flags var_indexes var (GetValueExpr idx))
          se;
        pr "@]@;}";
        pr "@]@;}@;")
      else generate_m_assign dgfip_flags var_indexes var None fmt se
  | Some ei ->
      Format.fprintf fmt "@[<v 2>{@,";
      let idx_val = fresh_c_local "mpp_idx" in
      let idx_def = idx_val ^ "_d" in
      let locals_idx, def_idx, value_idx =
        D.build_expression @@ generate_c_expr ei var_indexes
      in
      Format.fprintf fmt "char %s;@;long %s;@;%a%a@;%a" idx_def idx_val
        D.format_local_declarations locals_idx
        (D.format_assign dgfip_flags var_indexes idx_def)
        def_idx
        (D.format_assign dgfip_flags var_indexes idx_val)
        value_idx;
      let size = Dgfip_varid.gen_size var_indexes var in
      Format.fprintf fmt "@[<hov 2>if(%s && 0 <= %s && %s < %s){@,%a@]@,}"
        idx_def idx_val idx_val size
        (generate_m_assign dgfip_flags var_indexes var (GetValueExpr idx_val))
        (generate_c_expr vexpr var_indexes);
      Format.fprintf fmt "@]@,}"

let rec generate_stmt (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (var_indexes : Dgfip_varid.var_id_map)
    (oc : Format.formatter) (stmt : Mir.m_instruction) =
  match Pos.unmark stmt with
  | Affectation (SingleFormula (var, vidx_opt, vexpr), _) ->
      Format.fprintf oc "@[<v 2>{@,";
      generate_var_def dgfip_flags var_indexes var vidx_opt vexpr oc;
      Format.fprintf oc "@]@,}"
  | Affectation _ -> assert false
  | IfThenElse (cond, iftrue, iffalse) ->
      Format.fprintf oc "@[<v 2>{@,";
      let cond_val = fresh_c_local "mpp_cond" in
      let cond_def = cond_val ^ "_d" in
      let locals, def, value =
        D.build_expression @@ generate_c_expr cond var_indexes
      in
      Format.fprintf oc "char %s;@;double %s;@;%a%a@;%a" cond_def cond_val
        D.format_local_declarations locals
        (D.format_assign dgfip_flags var_indexes cond_def)
        def
        (D.format_assign dgfip_flags var_indexes cond_val)
        value;
      Format.fprintf oc "@[<hov 2>if(%s && %s){@,%a@]@,}" cond_def cond_val
        (generate_stmts dgfip_flags program var_indexes)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<hov 2>else if(%s){@,%a@]@,}" cond_def
          (generate_stmts dgfip_flags program var_indexes)
          iffalse;
      Format.fprintf oc "@]@,}"
  | VerifBlock stmts ->
      let goto_label = fresh_c_local "verif_block" in
      let pr fmt = Format.fprintf oc fmt in
      pr "@[<v 2>{@\n";
      pr "#ifdef FLG_MULTITHREAD@\n";
      pr "  if (setjmp(irdata->jmp_bloq) != 0) {@\n";
      pr "    goto %s;@\n" goto_label;
      pr "  }@\n";
      pr "#else@\n";
      pr "  if (setjmp(jmp_bloq) != 0) {@\n";
      pr "    goto %s;@\n" goto_label;
      pr "  }@\n";
      pr "#endif@\n";
      pr "%a@\n" (generate_stmts dgfip_flags program var_indexes) stmts;
      pr "%s:;@]@\n}@\n" goto_label
  | ComputeTarget (f, _) -> Format.fprintf oc "%s(irdata);" f
  | Print (std, args) ->
      let print_std, pr_ctx =
        match std with
        | StdOut -> ("stdout", "&(irdata->ctx_pr_out)")
        | StdErr -> ("stderr", "&(irdata->ctx_pr_err)")
      in
      let print_val = fresh_c_local "mpp_print" in
      let print_def = print_val ^ "_d" in
      Format.fprintf oc "@[<v 2>{@,char %s;@;double %s;@;" print_def print_val;
      List.iter
        (fun (arg : Mir.Var.t Com.print_arg Pos.marked) ->
          match Pos.unmark arg with
          | PrintString s ->
              Format.fprintf oc "print_string(%s, %s, \"%s\");@;" print_std
                pr_ctx (str_escape s)
          | PrintName (var, _) -> begin
              match Mir.VariableMap.find var var_indexes with
              | Dgfip_varid.VarIterate (t, _, _) ->
                  Format.fprintf oc "print_string(%s, %s, %s->name);@;"
                    print_std pr_ctx t
              | _ -> assert false
            end
          | PrintAlias (var, _) -> begin
              match Mir.VariableMap.find var var_indexes with
              | Dgfip_varid.VarIterate (t, _, _) ->
                  Format.fprintf oc "print_string(%s, %s, %s->alias);@;"
                    print_std pr_ctx t
              | _ -> assert false
            end
          | PrintIndent e ->
              let locals, def, value =
                D.build_expression @@ generate_c_expr e var_indexes
              in
              Format.fprintf oc "@[<v 2>{%a%a@;%a@;@]}@;"
                D.format_local_declarations locals
                (D.format_assign dgfip_flags var_indexes print_def)
                def
                (D.format_assign dgfip_flags var_indexes print_val)
                value;
              Format.fprintf oc "@[<v 2>if(%s){@;" print_def;
              Format.fprintf oc "set_print_indent(%s, %s, %s);@]@;" print_std
                pr_ctx print_val;
              Format.fprintf oc "}@;"
          | PrintExpr (e, min, max) ->
              let locals, def, value =
                D.build_expression @@ generate_c_expr e var_indexes
              in
              Format.fprintf oc "@[<v 2>{%a%a@;%a@;@]}@;"
                D.format_local_declarations locals
                (D.format_assign dgfip_flags var_indexes print_def)
                def
                (D.format_assign dgfip_flags var_indexes print_val)
                value;
              Format.fprintf oc "@[<v 2>if(%s){@;" print_def;
              Format.fprintf oc "print_double(%s, %s, %s, %d, %d);@]@;"
                print_std pr_ctx print_val min max;
              Format.fprintf oc "@[<v 2>} else {@;";
              Format.fprintf oc "print_string(%s, %s, \"indefini\");@]@;}@;"
                print_std pr_ctx)
        args;
      Format.fprintf oc "@]@;}@;"
  | Iterate (var, vcs, expr, stmts) ->
      let it_name = fresh_c_local "iterate" in
      Com.CatVar.Map.iter
        (fun vc _ ->
          let vcd = Com.CatVar.Map.find vc program.program_var_categories in
          let var_indexes =
            Mir.VariableMap.add var
              (Dgfip_varid.VarIterate ("tab_" ^ it_name, vcd.loc, vcd))
              var_indexes
          in
          Format.fprintf oc "@[<v 2>{@;";
          Format.fprintf oc
            "T_varinfo_%s *tab_%s = varinfo_%s;@;int nb_%s = 0;@;" vcd.id_str
            it_name vcd.id_str it_name;
          Format.fprintf oc "@[<v 2>while (nb_%s < NB_%s) {@;" it_name
            vcd.id_str;
          let cond_val = "cond_" ^ it_name in
          let cond_def = cond_val ^ "_d" in
          let locals, def, value =
            D.build_expression @@ generate_c_expr expr var_indexes
          in
          Format.fprintf oc "char %s;@;double %s;@;@[<v 2>{@;%a%a@;%a@]@;}@;"
            cond_def cond_val D.format_local_declarations locals
            (D.format_assign dgfip_flags var_indexes cond_def)
            def
            (D.format_assign dgfip_flags var_indexes cond_val)
            value;
          Format.fprintf oc "@[<hov 2>if(%s && %s){@;%a@]@;}@;" cond_def
            cond_val
            (generate_stmts dgfip_flags program var_indexes)
            stmts;
          Format.fprintf oc "tab_%s++;@;nb_%s++;@;" it_name it_name;
          Format.fprintf oc "@]}@;";
          Format.fprintf oc "@]}@;")
        vcs
  | Restore (vars, var_params, stmts) ->
      Format.fprintf oc "@[<v 2>{@;";
      let rest_name = fresh_c_local "restore" in
      Format.fprintf oc "T_env_sauvegarde *%s = NULL;@;" rest_name;
      List.iter
        (fun v ->
          Format.fprintf oc "env_sauvegarder(&%s, %s, %s, %s);@;" rest_name
            (Dgfip_varid.gen_access_def_pointer var_indexes v)
            (Dgfip_varid.gen_access_pointer var_indexes v)
            (Dgfip_varid.gen_size var_indexes v))
        vars;
      List.iter
        (fun (var, vcs, expr) ->
          let it_name = fresh_c_local "iterate" in
          Com.CatVar.Map.iter
            (fun vc _ ->
              let vcd = Com.CatVar.Map.find vc program.program_var_categories in
              let var_indexes =
                Mir.VariableMap.add var
                  (Dgfip_varid.VarIterate ("tab_" ^ it_name, vcd.loc, vcd))
                  var_indexes
              in
              Format.fprintf oc "@[<v 2>{@;";
              Format.fprintf oc
                "T_varinfo_%s *tab_%s = varinfo_%s;@;int nb_%s = 0;@;"
                vcd.id_str it_name vcd.id_str it_name;
              Format.fprintf oc "@[<v 2>while (nb_%s < NB_%s) {@;" it_name
                vcd.id_str;
              let cond_val = "cond_" ^ it_name in
              let cond_def = cond_val ^ "_d" in
              let locals, def, value =
                D.build_expression @@ generate_c_expr expr var_indexes
              in
              Format.fprintf oc
                "char %s;@;double %s;@;@[<v 2>{@;%a%a@;%a@]@;}@;" cond_def
                cond_val D.format_local_declarations locals
                (D.format_assign dgfip_flags var_indexes cond_def)
                def
                (D.format_assign dgfip_flags var_indexes cond_val)
                value;
              Format.fprintf oc "@[<hov 2>if(%s && %s){@;" cond_def cond_val;
              Format.fprintf oc "env_sauvegarder(&%s, %s, %s, %s);@;" rest_name
                (Dgfip_varid.gen_access_def_pointer var_indexes var)
                (Dgfip_varid.gen_access_pointer var_indexes var)
                (Dgfip_varid.gen_size var_indexes var);
              Format.fprintf oc "@]@;}@;";
              Format.fprintf oc "tab_%s++;@;nb_%s++;@;" it_name it_name;
              Format.fprintf oc "@]}@;";
              Format.fprintf oc "@]}@;")
            vcs)
        var_params;
      Format.fprintf oc "%a@;"
        (generate_stmts dgfip_flags program var_indexes)
        stmts;
      Format.fprintf oc "env_restaurer(&%s);@;" rest_name;
      Format.fprintf oc "@]}@;"
  | RaiseError (err, var_opt) ->
      let err_name = Pos.unmark err.Com.Error.name in
      let code =
        match var_opt with
        | Some var -> Format.sprintf "\"%s\"" (Pos.unmark var)
        | None -> "NULL"
      in
      Format.fprintf oc "add_erreur(irdata, &erreur_%s, %s);@;" err_name code
  | CleanErrors -> Format.fprintf oc "nettoie_erreur(irdata);@;"
  | ExportErrors -> Format.fprintf oc "exporte_erreur(irdata);@;"
  | FinalizeErrors -> Format.fprintf oc "finalise_erreur(irdata);@;"
  | ComputeDomain _ | ComputeChaining _ | ComputeVerifs _ -> assert false

and generate_stmts (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter)
    (stmts : Mir.m_instruction list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list (generate_stmt dgfip_flags program var_indexes) oc stmts;
  Format.fprintf oc "@]"

let generate_target_prototype (add_semicolon : bool) (oc : Format.formatter)
    (function_name : string) =
  Format.fprintf oc "struct S_discord * %s(T_irdata* irdata)%s" function_name
    (if add_semicolon then ";" else "")

let generate_var_tmp_decls (oc : Format.formatter)
    (tmp_vars : (Mir.Var.t * Pos.t * int option) StrMap.t) =
  StrMap.iter
    (fun vn (_, _, size) ->
      let sz = match size with Some i -> i | None -> 1 in
      Format.fprintf oc "char %s_def[%d];@,double %s_val[%d];@," vn sz vn sz)
    tmp_vars;
  if not (StrMap.is_empty tmp_vars) then Format.fprintf oc "@,";
  StrMap.iter
    (fun vn (_, _, size) ->
      match size with
      | Some 1 | None ->
          Format.fprintf oc "%s_def[0] = 0;@,%s_val[0] = 0.0;@," vn vn
      | Some i ->
          Format.fprintf oc "@[<v 2>{@;";
          Format.fprintf oc "int i;@;";
          Format.fprintf oc "for (i = 0; i < %d; i++) {@;" i;
          Format.fprintf oc "%s_def[i] = 0;@,%s_val[i] = 0.0;@," vn vn;
          Format.fprintf oc "@]@;}@;";
          Format.fprintf oc "@]@;}@;")
    tmp_vars;
  if not (StrMap.is_empty tmp_vars) then Format.fprintf oc "@,"

let generate_target (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (var_indexes : Dgfip_varid.var_id_map) (oc : Format.formatter) (f : string)
    =
  let Mir.{ target_tmp_vars; target_prog; _ } =
    Mir.TargetMap.find f program.program_targets
  in
  Format.fprintf oc "@[<v 2>%a{@,%a%s@\n%a%s@\n%s@]@,}@,"
    (generate_target_prototype false)
    f generate_var_tmp_decls target_tmp_vars
    (if dgfip_flags.flg_trace then "aff1(\"debut " ^ f ^ "\\n\") ;" else "")
    (generate_stmts dgfip_flags program var_indexes)
    target_prog
    (if dgfip_flags.flg_trace then "aff1(\"fin " ^ f ^ "\\n\") ;" else "")
    {|
#ifdef FLG_MULTITHREAD
      return irdata->discords;
#else
      return discords;
#endif
|}

let generate_targets (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (filemap : (out_channel * Format.formatter) StrMap.t)
    (var_indexes : Dgfip_varid.var_id_map) =
  let targets = Mir.TargetMap.bindings program.program_targets in
  List.iter
    (fun (name, Mir.{ target_file; _ }) ->
      let file_str = match target_file with Some s -> s | None -> "" in
      let _, fmt = StrMap.find file_str filemap in
      generate_target
        (dgfip_flags : Dgfip_options.flags)
        program var_indexes fmt name)
    targets

let generate_implem_header oc msg =
  Format.fprintf oc {|
/* %s */

#include <string.h>

#include "mlang.h"


|}
    msg

let generate_c_program (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (filename : string) (vm : Dgfip_varid.var_id_map) :
    unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let folder = Filename.dirname filename in
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a@\n@." generate_implem_header Prelude.message;
  let filemap =
    Mir.TargetMap.fold
      (fun _ (t : Mir.target_data) filemap ->
        let file_str = match t.target_file with Some s -> s | None -> "" in
        let update = function
          | Some fmt -> Some fmt
          | None ->
              let fn = Filename.concat folder (file_str ^ ".c") in
              let oc = open_out fn in
              let fmt = Format.formatter_of_out_channel oc in
              Format.fprintf fmt "#include \"mlang.h\"\n\n";
              Some (oc, fmt)
        in
        StrMap.update file_str update filemap)
      program.program_targets
      (StrMap.one "" (_oc, oc))
  in
  generate_targets dgfip_flags program filemap vm;
  StrMap.iter
    (fun _ (oc, fmt) ->
      Format.fprintf fmt "\n@?";
      close_out oc)
    filemap
