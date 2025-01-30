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
module VID = Dgfip_varid

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

let rec generate_c_expr (e : Mir.expression Pos.marked) :
    D.expression_composition =
  let comparison op se1 se2 =
    let safe_def = false in
    let set_vars = se1.D.set_vars @ se2.D.set_vars in
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
    D.build_transitive_composition ~safe_def { set_vars; def_test; value_comp }
  in
  let binop op se1 se2 =
    match Pos.unmark op with
    | _ ->
        let set_vars = se1.D.set_vars @ se2.D.set_vars in
        let def_test =
          match Pos.unmark op with
          | Com.And | Com.Mul | Com.Div | Com.Mod ->
              D.dand se1.def_test se2.def_test
          | Com.Or | Com.Add | Com.Sub -> D.dor se1.def_test se2.def_test
        in
        let op e1 e2 =
          match Pos.unmark op with
          | Com.And -> D.dand e1 e2
          | Com.Or -> D.dor e1 e2
          | Com.Add -> D.plus e1 e2
          | Com.Sub -> D.sub e1 e2
          | Com.Mul -> D.mult e1 e2
          | Com.Div -> D.ite e2 (D.div e1 e2) (D.lit 0.)
          | Com.Mod -> D.ite e2 (D.modulo e1 e2) (D.lit 0.)
        in
        let value_comp = op se1.value_comp se2.value_comp in
        D.build_transitive_composition ~safe_def:true
          { set_vars; def_test; value_comp }
  in
  let unop op se =
    let set_vars = se.D.set_vars in
    let def_test = se.def_test in
    let op, safe_def =
      match op with Com.Not -> (D.dnot, false) | Com.Minus -> (D.minus, true)
    in
    let value_comp = op se.value_comp in
    D.build_transitive_composition ~safe_def { set_vars; def_test; value_comp }
  in
  match Pos.unmark e with
  | Com.TestInSet (positive, e0, values) ->
      let se0 = generate_c_expr e0 in
      let ldef, lval = D.locals_from_m () in
      let sle0 =
        {
          D.set_vars = [];
          D.def_test = D.local_var ldef;
          D.value_comp = D.local_var lval;
        }
      in
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
                    D.{ set_vars = []; def_test; value_comp }
                  in
                  comparison (Com.Eq, Pos.no_pos) sle0 s_set_var
              | Com.FloatValue i ->
                  let s_i =
                    {
                      D.set_vars = [];
                      D.def_test = D.dtrue;
                      D.value_comp = D.lit (Pos.unmark i);
                    }
                  in
                  comparison (Com.Eq, Pos.no_pos) sle0 s_i
              | Com.Interval (bn, en) ->
                  let s_bn =
                    let bn' = float_of_int (Pos.unmark bn) in
                    D.{ set_vars = []; def_test = dtrue; value_comp = lit bn' }
                  in
                  let s_en =
                    let en' = float_of_int (Pos.unmark en) in
                    D.{ set_vars = []; def_test = dtrue; value_comp = lit en' }
                  in
                  binop (Com.And, Pos.no_pos)
                    (comparison (Com.Gte, Pos.no_pos) sle0 s_bn)
                    (comparison (Com.Lte, Pos.no_pos) sle0 s_en)
            in
            binop (Com.Or, Pos.no_pos) or_chain equal_test)
          D.{ set_vars = []; def_test = dfalse; value_comp = lit 0. }
          values
      in
      let se = if positive then or_chain else unop Com.Not or_chain in
      {
        D.set_vars = se0.set_vars;
        D.def_test = declare_local se.def_test;
        D.value_comp = declare_local se.value_comp;
      }
  | Comparison (op, e1, e2) ->
      let se1 = generate_c_expr e1 in
      let se2 = generate_c_expr e2 in
      comparison op se1 se2
  | Binop (op, e1, e2) ->
      let se1 = generate_c_expr e1 in
      let se2 = generate_c_expr e2 in
      binop op se1 se2
  | Unop (op, e) ->
      let se = generate_c_expr e in
      unop op se
  | Index (var, e) ->
      let index = fresh_c_local "index" in
      let def_index = Pp.spr "def_%s" index in
      let val_index = Pp.spr "val_%s" index in
      let idx = generate_c_expr e in
      let size = VID.gen_size (Pos.unmark var) in
      let set_vars =
        idx.D.set_vars
        @ [
            (D.Def, def_index, idx.def_test); (D.Val, val_index, idx.value_comp);
          ]
      in
      let def_test =
        D.dand
          (D.dand (D.dinstr def_index)
             (D.comp "<" (D.dinstr val_index) (D.dinstr size)))
          (D.access (Pos.unmark var) Def (D.dinstr val_index))
      in
      let value_comp =
        D.ite
          (D.comp "<" (D.dinstr val_index) (D.lit 0.))
          (D.lit 0.)
          (D.access (Pos.unmark var) Val (D.dinstr val_index))
      in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | Conditional (c, t, f_opt) ->
      let cond = generate_c_expr c in
      let thenval = generate_c_expr t in
      let elseval =
        match f_opt with
        | None -> D.{ set_vars = []; def_test = dfalse; value_comp = lit 0. }
        | Some f -> generate_c_expr f
      in
      let set_vars =
        cond.D.set_vars @ thenval.D.set_vars @ elseval.D.set_vars
      in
      let def_test =
        D.dand cond.def_test
          (D.ite cond.value_comp thenval.def_test elseval.def_test)
      in
      let value_comp =
        D.ite cond.value_comp thenval.value_comp elseval.value_comp
      in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | FuncCall ((Supzero, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let cond = D.dand se.def_test (D.comp ">=" se.value_comp (D.lit 0.0)) in
      let def_test = D.ite cond D.dfalse se.def_test in
      let value_comp = D.ite cond (D.lit 0.0) se.value_comp in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | FuncCall ((PresentFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = D.dtrue in
      let value_comp = se.def_test in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((NullFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp =
        D.dand def_test (D.comp "==" se.value_comp (D.lit 0.0))
      in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((ArrFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_arr" [ se.value_comp ] in
      (* Here we boldly assume that rounding value of `undef` will give zero,
         given the invariant. Pretty sure that not true, in case of doubt, turn
         `safe_def` to false *)
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((InfFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_floor" [ se.value_comp ] in
      (* same as above *)
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((AbsFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp = D.dfun "fabs" [ se.value_comp ] in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((MaxFunc, _), [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 in
      let se2 = generate_c_expr e2 in
      let set_vars = se1.D.set_vars @ se2.D.set_vars in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "max" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((MinFunc, _), [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 in
      let se2 = generate_c_expr e2 in
      let set_vars = se1.D.set_vars @ se2.D.set_vars in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "min" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall ((Multimax, _), [ e1; (Var v2, _) ]) ->
      let bound = generate_c_expr e1 in
      let set_vars = bound.D.set_vars in
      let def_test =
        D.dfun "multimax_def" [ bound.value_comp; D.m_var v2 PassPointer Def ]
      in
      let value_comp =
        D.dfun "multimax" [ bound.value_comp; D.m_var v2 PassPointer Val ]
      in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | FuncCall ((NbEvents, _), _) ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_evenements(irdata)" in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | FuncCall ((Func fn, _), args) ->
      let res = fresh_c_local "result" in
      let def_res = Pp.spr "def_%s" res in
      let val_res = Pp.spr "val_%s" res in
      let def_res_ptr = Pp.spr "&%s" def_res in
      let val_res_ptr = Pp.spr "&%s" val_res in
      let set_vars, arg_exprs =
        let rec aux (set_vars, arg_exprs) = function
          | [] -> (List.rev set_vars, List.rev arg_exprs)
          | a :: la ->
              let e = generate_c_expr a in
              let set_vars = List.rev e.set_vars @ set_vars in
              let arg_exprs = e.value_comp :: e.def_test :: arg_exprs in
              aux (set_vars, arg_exprs) la
        in
        aux ([], []) args
      in
      let d_fun =
        D.dfun fn
          ([
             D.dlow_level "irdata";
             D.dlow_level def_res_ptr;
             D.dlow_level val_res_ptr;
           ]
          @ arg_exprs)
      in
      let set_vars =
        set_vars
        @ [ (D.Def, def_res, d_fun); (D.Val, val_res, D.dlow_level val_res) ]
      in
      let def_test = D.dinstr def_res in
      let value_comp = D.dinstr val_res in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | FuncCall _ -> assert false (* should not happen *)
  | Literal (Float f) ->
      { set_vars = []; def_test = D.dtrue; value_comp = D.lit f }
  | Literal Undefined ->
      { set_vars = []; def_test = D.dfalse; value_comp = D.lit 0. }
  | Var var ->
      {
        set_vars = [];
        def_test = D.m_var var None Def;
        value_comp = D.m_var var None Val;
      }
  | Attribut (var, a) ->
      let ptr = VID.gen_info_ptr (Pos.unmark var) in
      let def_test =
        D.dinstr
          (Format.sprintf "attribut_%s_def((T_varinfo *)%s)" (Pos.unmark a) ptr)
      in
      let value_comp =
        D.dinstr
          (Format.sprintf "attribut_%s((T_varinfo *)%s)" (Pos.unmark a) ptr)
      in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | EventField (me, f, _) ->
      let fn = Format.sprintf "event_field_%s" (Pos.unmark f) in
      let res = fresh_c_local "result" in
      let def_res = Pp.spr "def_%s" res in
      let val_res = Pp.spr "val_%s" res in
      let def_res_ptr = Pp.spr "&%s" def_res in
      let val_res_ptr = Pp.spr "&%s" val_res in
      let set_vars, arg_exprs =
        let e = generate_c_expr me in
        (e.set_vars, [ e.def_test; e.value_comp ])
      in
      let d_fun =
        D.dfun fn
          ([
             D.dlow_level "irdata";
             D.dlow_level def_res_ptr;
             D.dlow_level val_res_ptr;
           ]
          @ arg_exprs)
      in
      let set_vars =
        set_vars
        @ [ (D.Def, def_res, d_fun); (D.Val, val_res, D.dlow_level val_res) ]
      in
      let def_test = D.dinstr def_res in
      let value_comp = D.dinstr val_res in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | Size var ->
      let ptr = VID.gen_info_ptr (Pos.unmark var) in
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr (Format.sprintf "(%s->size)" ptr) in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | NbAnomalies ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_anomalies(irdata)" in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | NbDiscordances ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_discordances(irdata)" in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | NbInformatives ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_informatives(irdata)" in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | NbBloquantes ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_bloquantes(irdata)" in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | NbCategory _ | FuncCallLoop _ | Loop _ -> assert false

let generate_expr_with_res_in dgfip_flags oc res_def res_val expr =
  let pr form = Format.fprintf oc form in
  let locals, set, def, value = D.build_expression @@ generate_c_expr expr in
  if D.is_always_true def then
    pr "@;@[<v 2>{@;%a@;%a@;%a@;%a@]@;}" D.format_local_declarations locals
      (D.format_set_vars dgfip_flags)
      set
      (D.format_assign dgfip_flags res_def)
      def
      (D.format_assign dgfip_flags res_val)
      value
  else
    pr "@;@[<v 2>{@;%a@;%a@;%a@;@[<v 2>if (%s) {%a@]@;} else %s = 0.0;@]@;}"
      D.format_local_declarations locals
      (D.format_set_vars dgfip_flags)
      set
      (D.format_assign dgfip_flags res_def)
      def res_def
      (D.format_assign dgfip_flags res_val)
      value res_val

let generate_m_assign (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (offset : D.offset) (oc : Format.formatter)
    (expr : Mir.expression Pos.marked) : unit =
  let var_def = D.generate_variable ~def_flag:true offset var in
  let var_val = D.generate_variable offset var in
  generate_expr_with_res_in dgfip_flags oc var_def var_val expr;
  (* If the trace flag is set, we print the value of all non-temp variables *)
  if dgfip_flags.flg_trace && not (Com.Var.is_temp var) then
    Format.fprintf oc "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Com.Var.name)
      (VID.gen_pos_from_start var)

let generate_var_def (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (vidx_opt : Mir.expression Pos.marked option)
    (vexpr : Mir.expression Pos.marked) (oc : Format.formatter) : unit =
  let pr form = Format.fprintf oc form in
  let size = VID.gen_size var in
  match vidx_opt with
  | None ->
      if Com.Var.is_ref var then (
        pr "@;@[<v 2>{";
        let idx = fresh_c_local "idx" in
        pr "@;int %s;" idx;
        pr "@;@[<v 2>for (%s = 0; %s < %s; %s++) {" idx idx size idx;
        pr "%a" (generate_m_assign dgfip_flags var (GetValueExpr idx)) vexpr;
        pr "@]@;}";
        pr "@]@;}")
      else generate_m_assign dgfip_flags var None oc vexpr
  | Some ei ->
      pr "@;@[<v 2>{";
      let idx = fresh_c_local "idx" in
      let idx_def = idx ^ "_def" in
      let idx_val = idx ^ "_val" in
      pr "@;char %s;@;double %s;@;int %s;" idx_def idx_val idx;
      generate_expr_with_res_in dgfip_flags oc idx_def idx_val ei;
      pr "@;%s = (int)%s;" idx idx_val;
      pr "@;@[<v 2>if (%s && 0 <= %s && %s < %s) {" idx_def idx idx size;
      pr "%a" (generate_m_assign dgfip_flags var (GetValueExpr idx)) vexpr;
      pr "@]@;}";
      pr "@]@;}"

let generate_event_field_def (dgfip_flags : Dgfip_options.flags)
    (p : Mir.program) (idx_expr : Mir.expression Pos.marked) (field : string)
    (expr : Mir.expression Pos.marked) (oc : Format.formatter) : unit =
  let pr form = Format.fprintf oc form in
  pr "@;@[<v 2>{";
  let idx = fresh_c_local "idx" in
  let idx_def = idx ^ "_def" in
  let idx_val = idx ^ "_val" in
  pr "@;char %s;@;double %s;@;int %s;" idx_def idx_val idx;
  generate_expr_with_res_in dgfip_flags oc idx_def idx_val idx_expr;
  pr "@;%s = (int)%s;" idx idx_val;
  pr "@;@[<v 2>if (%s && 0 <= %s && %s < irdata->nb_events) {" idx_def idx idx;
  let res = fresh_c_local "res" in
  let res_def = res ^ "_def" in
  let res_val = res ^ "_val" in
  pr "@;char %s;@;double %s;" res_def res_val;
  generate_expr_with_res_in dgfip_flags oc res_def res_val expr;
  if (StrMap.find field p.program_event_fields).is_var then
    pr "@;ecris_varinfo(irdata, irdata->events[%s]->field_%s_var, %s, %s);" idx
      field res_def res_val
  else (
    pr "@;irdata->events[%s]->field_%s_def = %s;" idx field res_def;
    pr "@;irdata->events[%s]->field_%s_val = %s;" idx field res_val);
  pr "@]@;}";
  pr "@]@;}"

let rec generate_stmt (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (oc : Format.formatter) (stmt : Mir.m_instruction) =
  let pr fmt = Format.fprintf oc fmt in
  match Pos.unmark stmt with
  | Affectation (SingleFormula (VarDecl (m_var, vidx_opt, vexpr)), _) ->
      pr "@;@[<v 2>{";
      generate_var_def dgfip_flags (Pos.unmark m_var) vidx_opt vexpr oc;
      pr "@]@;}"
  | Affectation (SingleFormula (EventFieldDecl (idx, f, _, expr)), _) ->
      pr "@;@[<v 2>{";
      generate_event_field_def dgfip_flags program idx (Pos.unmark f) expr oc;
      pr "@]@;}"
  | Affectation (MultipleFormulaes _, _) -> assert false
  | IfThenElse (cond_expr, iftrue, iffalse) ->
      pr "@;@[<v 2>{";
      let cond = fresh_c_local "cond" in
      let cond_def = cond ^ "_def" in
      let cond_val = cond ^ "_val" in
      pr "@;char %s;@;double %s;" cond_def cond_val;
      generate_expr_with_res_in dgfip_flags oc cond_def cond_val cond_expr;
      pr "@;@[<v 2>if (%s && (%s != 0.0)) {" cond_def cond_val;
      pr "@;%a" (generate_stmts dgfip_flags program) iftrue;
      if iffalse <> [] then (
        pr "@]@;@[<v 2>} else if (%s) {" cond_def;
        pr "@;%a" (generate_stmts dgfip_flags program) iffalse);
      pr "@]@;}";
      pr "@]@;}"
  | WhenDoElse (wdl, ed) ->
      let goto_label = fresh_c_local "when_do_block" in
      let fin_label = fresh_c_local "when_do_end" in
      let cond = fresh_c_local "when_do_cond" in
      let cond_def = cond ^ "_def" in
      let cond_val = cond ^ "_val" in
      pr "@;@[<v 2>{";
      pr "@;char %s;@;double %s;" cond_def cond_val;
      let rec aux = function
        | (expr, dl, _) :: l ->
            generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
            pr "@;@[<v 2>if(%s) {" cond_def;
            pr "@;if (! %s) goto %s;" cond_val goto_label;
            pr "@;%a" (generate_stmts dgfip_flags program) dl;
            pr "@]@;}";
            aux l
        | [] -> ()
      in
      aux wdl;
      pr "@;goto %s;" fin_label;
      pr "@;%s:" goto_label;
      pr "@;%a" (generate_stmts dgfip_flags program) (Pos.unmark ed);
      pr "@;%s:{}" fin_label;
      pr "@]@;}"
  | VerifBlock stmts ->
      let goto_label = fresh_c_local "verif_block" in
      pr "@;@[<v 2>{";
      pr "@;if (setjmp(irdata->jmp_bloq) != 0) goto %s;" goto_label;
      pr "@;%a" (generate_stmts dgfip_flags program) stmts;
      pr "%s:;" goto_label;
      pr "@]@;}"
  | Print (std, args) ->
      let print_std, pr_ctx =
        match std with
        | StdOut -> ("stdout", "&(irdata->ctx_pr_out)")
        | StdErr -> ("stderr", "&(irdata->ctx_pr_err)")
      in
      let print = fresh_c_local "print" in
      let print_def = print ^ "_def" in
      let print_val = print ^ "_val" in
      pr "@;@[<v 2>{";
      pr "@;char %s;@;double %s;@;int %s;" print_def print_val print;
      let print_name_or_alias name_or_alias e f =
        let ef = StrMap.find (Pos.unmark f) program.program_event_fields in
        if ef.is_var then (
          generate_expr_with_res_in dgfip_flags oc print_def print_val e;
          pr "@;%s = (int)%s;" print print_val;
          pr "@;@[<v 2>if (%s && 0 <= %s && %s < irdata->nb_events) {" print_def
            print print;
          pr "@;print_string(%s, %s, irdata->events[%s]->field_%s_var->%s);"
            print_std pr_ctx print (Pos.unmark f) name_or_alias;
          pr "@]@;}")
      in
      List.iter
        (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
          match Pos.unmark arg with
          | PrintString s ->
              pr "@;print_string(%s, %s, \"%s\");" print_std pr_ctx
                (str_escape s)
          | PrintName (var, _) ->
              let ptr = VID.gen_info_ptr var in
              pr "@;print_string(%s, %s, %s->name);" print_std pr_ctx ptr
          | PrintAlias (var, _) ->
              let ptr = VID.gen_info_ptr var in
              pr "@;print_string(%s, %s, %s->alias);" print_std pr_ctx ptr
          | PrintEventName (e, f, _) -> print_name_or_alias "name" e f
          | PrintEventAlias (e, f, _) -> print_name_or_alias "alias" e f
          | PrintIndent e ->
              generate_expr_with_res_in dgfip_flags oc print_def print_val e;
              pr "@;@[<v 2>if (%s) {" print_def;
              pr "@;set_print_indent(%s, %s, %s);" print_std pr_ctx print_val;
              pr "@]@;}"
          | PrintExpr (e, min, max) ->
              generate_expr_with_res_in dgfip_flags oc print_def print_val e;
              pr "@;@[<v 2>if (%s) {" print_def;
              pr "@;print_double(%s, %s, %s, %d, %d);" print_std pr_ctx
                print_val min max;
              pr "@]@;@[<v 2>} else {";
              pr "@;print_string(%s, %s, \"indefini\");" print_std pr_ctx;
              pr "@]@;}")
        args;
      pr "@]@;}"
  | ComputeTarget ((tn, _), targs) ->
      ignore
        (List.fold_left
           (fun n ((v : Com.Var.t), _) ->
             let ref_idx = Format.sprintf "irdata->ref_org + %d" n in
             let ref_info = Format.sprintf "irdata->info_ref[%s]" ref_idx in
             let v_info_p = VID.gen_info_ptr v in
             pr "@;%s = %s;" ref_info v_info_p;
             let ref_def = Format.sprintf "irdata->def_ref[%s]" ref_idx in
             let v_def_p = VID.gen_def_ptr v in
             pr "@;%s = %s;" ref_def v_def_p;
             let ref_val = Format.sprintf "irdata->ref[%s]" ref_idx in
             let v_val_p = VID.gen_val_ptr v in
             pr "@;%s = %s;" ref_val v_val_p;
             n + 1)
           0 targs);
      pr "@;%s(irdata);" tn
  | Iterate (m_var, vars, var_params, stmts) ->
      let it_name = fresh_c_local "iterate" in
      let var = Pos.unmark m_var in
      let ref_info = VID.gen_info_ptr var in
      let ref_def = VID.gen_def_ptr var in
      let ref_val = VID.gen_val_ptr var in
      List.iter
        (fun (v, _) ->
          pr "@;@[<v 2>{";
          pr "@;%s = %s;" ref_info (VID.gen_info_ptr v);
          pr "@;%s = %s;" ref_def (VID.gen_def_ptr v);
          pr "@;%s = %s;" ref_val (VID.gen_val_ptr v);
          pr "@;%a" (generate_stmts dgfip_flags program) stmts;
          pr "@]@;}")
        vars;
      List.iter
        (fun (vcs, expr) ->
          Com.CatVar.Map.iter
            (fun vc _ ->
              let vcd = Com.CatVar.Map.find vc program.program_var_categories in
              let ref_tab = VID.gen_tab vcd.loc in
              let cond = fresh_c_local "cond" in
              let cond_def = cond ^ "_def" in
              let cond_val = cond ^ "_val" in
              pr "@;@[<v 2>{";
              pr "@;T_varinfo_%s *tab_%s = varinfo_%s;" vcd.id_str it_name
                vcd.id_str;
              pr "@;int nb_%s = 0;" it_name;
              pr "@;@[<v 2>while (nb_%s < NB_%s) {" it_name vcd.id_str;
              pr "@;char %s;@;double %s;" cond_def cond_val;
              pr "@;%s = (T_varinfo *)tab_%s;" ref_info it_name;
              pr "@;%s = &(D%s[%s->idx]);" ref_def ref_tab ref_info;
              pr "@;%s = &(%s[%s->idx]);" ref_val ref_tab ref_info;
              generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
              pr "@;@[<hov 2>if (%s && %s != 0.0) {" cond_def cond_val;
              pr "@;%a" (generate_stmts dgfip_flags program) stmts;
              pr "@]@;}";
              pr "@;tab_%s++;" it_name;
              pr "@;nb_%s++;" it_name;
              pr "@]@;}";
              pr "@]@;}")
            vcs)
        var_params
  | Iterate_values (m_var, var_intervals, stmts) ->
      let var = Pos.unmark m_var in
      let itval_def = VID.gen_def var "" in
      let itval_val = VID.gen_val var "" in
      let itval_name = fresh_c_local "itval" in
      let e0_def = Format.sprintf "%s_e0_def" itval_name in
      let e0_val = Format.sprintf "%s_e0_val" itval_name in
      let e1_def = Format.sprintf "%s_e1_def" itval_name in
      let e1_val = Format.sprintf "%s_e1_val" itval_name in
      let step_def = Format.sprintf "%s_step_def" itval_name in
      let step_val = Format.sprintf "%s_step_val" itval_name in
      List.iter
        (fun (e0, e1, step) ->
          pr "@;@[<v 2>{";
          pr "@;char %s;@;double %s;" e0_def e0_val;
          pr "@;char %s;@;double %s;" e1_def e1_val;
          pr "@;char %s;@;double %s;" step_def step_val;
          generate_expr_with_res_in dgfip_flags oc e0_def e0_val e0;
          generate_expr_with_res_in dgfip_flags oc e1_def e1_val e1;
          generate_expr_with_res_in dgfip_flags oc step_def step_val step;
          pr "@;@[<v 2>if(%s && %s && %s && %s != 0.0){" e0_def e1_def step_def
            step_val;
          pr
            "@;\
             @[<v 2>for (%s = 1, %s = %s; (%s > 0.0 ? %s <= %s : %s >= %s); %s \
             = %s + %s) {"
            itval_def itval_val e0_val step_val itval_val e1_val itval_val
            e1_val itval_val itval_val step_val;
          pr "@;%a" (generate_stmts dgfip_flags program) stmts;
          pr "@]@;}";
          pr "@]@;}";
          pr "@]@;}")
        var_intervals
  | ArrangeEvents (sort, filter, stmts) ->
      let events_sav = fresh_c_local "events_sav" in
      let events_tmp = fresh_c_local "events_tmp" in
      let nb_events_sav = fresh_c_local "nb_events_sav" in
      let cpt_i = fresh_c_local "i" in
      let cpt_j = fresh_c_local "j" in
      pr "@;@[<v 2>{";
      pr "@;T_event **%s = irdata->events;" events_sav;
      pr "@;int %s = irdata->nb_events;" nb_events_sav;
      pr "@;T_event **%s = NULL;" events_tmp;
      pr "@;int %s = 0;" cpt_i;
      pr "@;int %s = 0;" cpt_j;
      pr "@;%s = (T_event **)malloc(%s * (sizeof (T_event *)));" events_tmp
        nb_events_sav;
      (match filter with
      | Some (m_var, expr) ->
          pr "@;@[<v 2>while(%s < %s) {" cpt_j nb_events_sav;
          let var = Pos.unmark m_var in
          let ref_def = VID.gen_def var "" in
          let ref_val = VID.gen_val var "" in
          let cond = fresh_c_local "cond" in
          let cond_def = cond ^ "_def" in
          let cond_val = cond ^ "_val" in
          pr "@;char %s;@;double %s;" cond_def cond_val;
          pr "@;%s = 1;" ref_def;
          pr "@;%s = (double)%s;" ref_val cpt_j;
          generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
          pr "@;@[<v 2>if (%s && %s != 0.0) {" cond_def cond_val;
          pr "@;%s[%s] = irdata->events[%s];" events_tmp cpt_i cpt_j;
          pr "@;%s++;" cpt_i;
          pr "@]@;}";
          pr "@;%s++;" cpt_j;
          pr "@]@;}";
          pr "@;irdata->events = %s;" events_tmp;
          pr "@;irdata->nb_events = %s;" cpt_i
      | None ->
          pr "@;@[<v 2>while (%s < %s) {" cpt_j nb_events_sav;
          pr "@;%s[%s] = irdata->events[%s];" events_tmp cpt_j cpt_j;
          pr "@;%s++;" cpt_j;
          pr "@]@;}";
          pr "@;irdata->events = %s;" events_tmp);
      (match sort with
      | Some (m_var0, m_var1, expr) ->
          pr "@;/* merge sort */";
          pr "@;@[<v 2>{";
          pr
            "@;\
             T_event **b = (T_event **)malloc(irdata->nb_events * (sizeof \
             (T_event *)));";
          pr "@;int width;";
          pr "@;int iLeft;";
          pr "@;int i;";
          pr
            "@;\
             @[<v 2>for (width = 1; width < irdata->nb_events; width = 2 * \
             width) {";
          pr
            "@;\
             @[<v 2>for (iLeft = 0; iLeft < irdata->nb_events; iLeft = iLeft + \
             2 * width) {";
          pr "@;int iRight = iLeft + width;";
          pr "@;int iEnd = iLeft + 2 * width;";
          pr "@;if (iRight > irdata->nb_events) iRight = irdata->nb_events;";
          pr "@;if (iEnd > irdata->nb_events) iEnd = irdata->nb_events;";
          pr "@;@[<v 2>{";
          pr "@;int i = iLeft;";
          pr "@;int j = iRight;";
          pr "@;int k;";
          pr "@;@[<v 2>for (k = iLeft; k < iEnd; k++) {";
          pr "@;int cpt = 0;";
          pr "@;@[<v 2>{";
          (* Comparaison *)
          let var0 = Pos.unmark m_var0 in
          let ref0_def = VID.gen_def var0 "" in
          let ref0_val = VID.gen_val var0 "" in
          let var1 = Pos.unmark m_var1 in
          let ref1_def = VID.gen_def var1 "" in
          let ref1_val = VID.gen_val var1 "" in
          let cmp_def = fresh_c_local "cmp_def" in
          let cmp_val = fresh_c_local "cmp_val" in
          pr "@;char %s;@;double %s;" cmp_def cmp_val;
          pr "@;%s = 1;" ref0_def;
          pr "@;%s = (double)i;" ref0_val;
          pr "@;%s = 1;" ref1_def;
          pr "@;%s = (double)j;" ref1_val;
          generate_expr_with_res_in dgfip_flags oc cmp_def cmp_val expr;
          pr "@;cpt = %s && %s != 0.0;" cmp_def cmp_val;
          (* ----------- *)
          pr "@]@;}";
          pr "@;@[<v 2>if (i < iRight && (j >= iEnd || cpt)) {";
          pr "@;b[k] = irdata->events[i];";
          pr "@;i = i + 1;";
          pr "@]@;@;@[<v 2>} else {";
          pr "@;b[k] = irdata->events[j];";
          pr "@;j = j + 1;";
          pr "@]@;}";
          pr "@]@;}";
          pr "@]@;}";
          pr "@]@;}";
          pr "@;@[<v 2>for (i = 0; i < irdata->nb_events; i++) {";
          pr "@;irdata->events[i] = b[i];";
          pr "@]@;}";
          pr "@]@;}";
          pr "@;free(b);";
          pr "@]@;}"
      | None -> ());
      pr "@;%a" (generate_stmts dgfip_flags program) stmts;
      pr "@;free(irdata->events);";
      pr "@;irdata->events = %s;" events_sav;
      pr "@;irdata->nb_events = %s;" nb_events_sav;
      pr "@]@;}"
  | Restore (vars, var_params, evts, evtfs, stmts) ->
      pr "@;@[<v 2>{";
      let rest_name = fresh_c_local "restore" in
      let rest_evt_name = fresh_c_local "restore_evt" in
      pr "@;T_env_sauvegarde *%s = NULL;" rest_name;
      pr "@;T_env_sauvegarde_evt *%s = NULL;" rest_evt_name;
      List.iter
        (fun m_v ->
          let v = Pos.unmark m_v in
          pr "@;env_sauvegarder(&%s, %s, %s, %s);" rest_name (VID.gen_def_ptr v)
            (VID.gen_val_ptr v) (VID.gen_size v))
        vars;
      List.iter
        (fun (m_var, vcs, expr) ->
          let var = Pos.unmark m_var in
          let it_name = fresh_c_local "iterate" in
          Com.CatVar.Map.iter
            (fun vc _ ->
              let vcd = Com.CatVar.Map.find vc program.program_var_categories in
              let ref_tab = VID.gen_tab vcd.loc in
              let ref_info = VID.gen_info_ptr var in
              let ref_def = VID.gen_def_ptr var in
              let ref_val = VID.gen_val_ptr var in
              let cond = fresh_c_local "cond" in
              let cond_def = cond ^ "_def" in
              let cond_val = cond ^ "_val" in
              pr "@;@[<v 2>{";
              pr "@;T_varinfo_%s *tab_%s = varinfo_%s;" vcd.id_str it_name
                vcd.id_str;
              pr "@;int nb_%s = 0;" it_name;
              pr "@;@[<v 2>while (nb_%s < NB_%s) {" it_name vcd.id_str;
              pr "@;char %s;@;double %s;" cond_def cond_val;
              pr "@;%s = (T_varinfo *)tab_%s;" ref_info it_name;
              pr "@;%s = &(D%s[%s->idx]);" ref_def ref_tab ref_info;
              pr "@;%s = &(%s[%s->idx]);" ref_val ref_tab ref_info;
              generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
              pr "@;@[<v 2>if (%s && %s != 0.0){" cond_def cond_val;
              pr "@;env_sauvegarder(&%s, %s, %s, %s);" rest_name
                (VID.gen_def_ptr var) (VID.gen_val_ptr var) (VID.gen_size var);
              pr "@]@;}";
              pr "@;tab_%s++;" it_name;
              pr "@;nb_%s++;" it_name;
              pr "@]@;}";
              pr "@]@;}")
            vcs)
        var_params;
      List.iter
        (fun expr ->
          let idx = fresh_c_local "idx" in
          let idx_def = idx ^ "_def" in
          let idx_val = idx ^ "_val" in
          pr "@;@[<v 2>{";
          pr "@;char %s;@;double %s;" idx_def idx_val;
          pr "@;int %s;" idx;
          generate_expr_with_res_in dgfip_flags oc idx_def idx_val expr;
          pr "@;%s = (int)%s;" idx idx_val;
          pr "@;@[<v 2>if (%s && 0 <= %s && %s < irdata->nb_events) {" idx_def
            idx idx;
          pr "@;env_sauvegarder_evt(&%s, irdata->events[%s]);@;" rest_evt_name
            idx;
          pr "@]@;}";
          pr "@]@;}")
        evts;
      List.iter
        (fun (m_var, expr) ->
          let var = Pos.unmark m_var in
          let idx = fresh_c_local "idx" in
          let ref_def = VID.gen_def var "" in
          let ref_val = VID.gen_val var "" in
          let cond = fresh_c_local "cond" in
          let cond_def = cond ^ "_def" in
          let cond_val = cond ^ "_val" in
          pr "@;@[<v 2>{";
          pr "@;int %s = 0;" idx;
          pr "@;@[<v 2>while (%s < irdata->nb_events) {" idx;
          pr "@;char %s;@;double %s;" cond_def cond_val;
          pr "@;%s = 1;" ref_def;
          pr "@;%s = (double)%s;" ref_val idx;
          generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
          pr "@;@[<v 2>if (%s && %s != 0.0){" cond_def cond_val;
          pr "@;env_sauvegarder_evt(&%s, irdata->events[%s]);@;" rest_evt_name
            idx;
          pr "@]@;}";
          pr "@;%s++;" idx;
          pr "@]@;}";
          pr "@]@;}")
        evtfs;
      pr "@;%a" (generate_stmts dgfip_flags program) stmts;
      pr "@;env_restaurer(&%s);@;" rest_name;
      pr "@;env_restaurer_evt(&%s);@;" rest_evt_name;
      pr "@]@;}"
  | RaiseError (m_err, var_opt) ->
      let err = Pos.unmark m_err in
      let err_name = Pos.unmark err.Com.Error.name in
      let code =
        match var_opt with
        | Some var -> Format.sprintf "\"%s\"" (Pos.unmark var)
        | None -> "NULL"
      in
      pr "@;add_erreur(irdata, &erreur_%s, %s);" err_name code
  | CleanErrors -> Format.fprintf oc "@;nettoie_erreur(irdata);"
  | ExportErrors -> Format.fprintf oc "@;exporte_erreur(irdata);"
  | FinalizeErrors -> Format.fprintf oc "@;finalise_erreur(irdata);"
  | ComputeDomain _ | ComputeChaining _ | ComputeVerifs _ -> assert false

and generate_stmts (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (oc : Format.formatter) (stmts : Mir.m_instruction list) =
  Format.pp_print_list (generate_stmt dgfip_flags program) oc stmts

let generate_var_tmp_decls (oc : Format.formatter) (tf : Mir.target_data) =
  let pr fmt = Format.fprintf oc fmt in
  if tf.target_sz_tmps > 0 then (
    pr "@;@[<v 2>{";
    pr "@;int i;";
    pr "@;T_varinfo *info;";
    pr "@;@[<v 2>for (i = 0; i < %d; i++) {" tf.target_sz_tmps;
    pr "@;irdata->def_tmps[irdata->tmps_org + i] = 0;";
    pr "@;irdata->tmps[irdata->tmps_org + i] = 0.0;";
    pr "@]@;}";
    pr "@;irdata->tmps_org = irdata->tmps_org + %d;" tf.target_sz_tmps;
    StrMap.iter
      (fun vn (var, _, sz_opt) ->
        let loc_str =
          Format.sprintf "irdata->tmps_org + (%d)" (Com.Var.loc_int var)
        in
        pr "@;info = &(irdata->info_tmps[%s]);" loc_str;
        pr "@;info->name = \"%s\";" vn;
        pr "@;info->alias = \"\";";
        pr "@;info->idx = %s;" loc_str;
        (match sz_opt with
        | None -> pr "@;info->size = 1;"
        | Some i -> pr "@;info->size = %d;" i);
        pr "@;info->cat = ID_TMP_VARS;";
        pr "@;info->loc_cat = EST_TEMPORAIRE;")
      tf.target_tmp_vars;
    pr "@]@;}");
  if tf.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org + %d;" tf.target_nb_refs

let generate_function_prototype (add_semicolon : bool) (oc : Format.formatter)
    (fd : Mir.target_data) =
  let fn = Pos.unmark fd.target_name in
  let pp_args fmt args =
    List.iteri
      (fun i _ -> Pp.fpr fmt ", char def_arg%d, double val_arg%d" i i)
      args
  in
  Format.fprintf oc
    "int %s(T_irdata* irdata, char *def_res, double *val_res%a)%s" fn pp_args
    fd.Mir.target_args
    (if add_semicolon then ";" else "")

let generate_function (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (oc : Format.formatter) (fn : string) =
  let pr fmt = Format.fprintf oc fmt in
  let fd = Com.TargetMap.find fn program.program_functions in
  pr "@;@[<v 2>%a {" (generate_function_prototype false) fd;
  pr "%a" generate_var_tmp_decls fd;
  pr "@;";
  if dgfip_flags.flg_trace then pr "@;aff1(\"debut %s\\n\");" fn;
  pr "%a" (generate_stmts dgfip_flags program) fd.target_prog;
  if dgfip_flags.flg_trace then pr "@;aff1(\"fin %s\\n\");" fn;
  pr "@;";
  if fd.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org - %d;" fd.target_nb_refs;
  if fd.target_sz_tmps > 0 then
    pr "@;irdata->tmps_org = irdata->tmps_org - %d;" fd.target_sz_tmps;
  pr "@;return 1;";
  pr "@]@;}@;"

let generate_functions (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program)
    (filemap : (out_channel * Format.formatter) StrMap.t) =
  let functions = Com.TargetMap.bindings program.program_functions in
  List.iter
    (fun (name, Mir.{ target_file; _ }) ->
      let file_str = match target_file with Some s -> s | None -> "" in
      let _, fmt = StrMap.find file_str filemap in
      generate_function (dgfip_flags : Dgfip_options.flags) program fmt name)
    functions

let generate_target_prototype (add_semicolon : bool) (oc : Format.formatter)
    (function_name : string) =
  Format.fprintf oc "struct S_discord * %s(T_irdata* irdata)%s" function_name
    (if add_semicolon then ";" else "")

let generate_target (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (oc : Format.formatter) (f : string) =
  let pr fmt = Format.fprintf oc fmt in
  let tf = Com.TargetMap.find f program.program_targets in
  pr "@;@[<v 2>%a {" (generate_target_prototype false) f;
  pr "%a" generate_var_tmp_decls tf;
  pr "@;";
  if dgfip_flags.flg_trace then pr "@;aff1(\"debut %s\\n\");" f;
  pr "%a" (generate_stmts dgfip_flags program) tf.target_prog;
  if dgfip_flags.flg_trace then pr "@;aff1(\"fin %s\\n\");" f;
  pr "@;";
  if tf.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org - %d;" tf.target_nb_refs;
  if tf.target_sz_tmps > 0 then
    pr "@;irdata->tmps_org = irdata->tmps_org - %d;" tf.target_sz_tmps;
  pr "@;return irdata->discords;";
  pr "@]@;}@;"

let generate_targets (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (filemap : (out_channel * Format.formatter) StrMap.t) =
  let targets = Com.TargetMap.bindings program.program_targets in
  List.iter
    (fun (name, Mir.{ target_file; _ }) ->
      let file_str = match target_file with Some s -> s | None -> "" in
      let _, fmt = StrMap.find file_str filemap in
      generate_target (dgfip_flags : Dgfip_options.flags) program fmt name)
    targets

let generate_implem_header oc msg =
  Format.fprintf oc {|
/* %s */

#include <string.h>

#include "mlang.h"


|}
    msg

let generate_c_program (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (filename : string) : unit =
  if Filename.extension filename <> ".c" then
    Errors.raise_error
      (Format.asprintf "Output file should have a .c extension (currently %s)"
         filename);
  let folder = Filename.dirname filename in
  let _oc = open_out filename in
  let oc = Format.formatter_of_out_channel _oc in
  Format.fprintf oc "%a@\n@." generate_implem_header Prelude.message;
  let filemap =
    Com.TargetMap.fold
      (fun _ (t : Mir.target_data) filemap ->
        let file_str = match t.target_file with Some s -> s | None -> "" in
        let update = function
          | Some fmt -> Some fmt
          | None ->
              let fn = Filename.concat folder (file_str ^ ".c") in
              let oc = open_out fn in
              let fmt = Format.formatter_of_out_channel oc in
              Format.fprintf fmt "#include \"mlang.h\"@;@;";
              Some (oc, fmt)
        in
        StrMap.update file_str update filemap)
      program.program_targets
      (StrMap.one "" (_oc, oc))
  in
  generate_functions dgfip_flags program filemap;
  generate_targets dgfip_flags program filemap;
  StrMap.iter
    (fun _ (oc, fmt) ->
      Format.fprintf fmt "@;@?";
      close_out oc)
    filemap
