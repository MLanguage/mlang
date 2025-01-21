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
          | Com.And | Com.Mul | Com.Div -> D.dand se1.def_test se2.def_test
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
  | EventField (me, f) ->
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

let generate_m_assign (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (offset : D.offset) (oc : Format.formatter) (se : D.expression_composition)
    : unit =
  let pr form = Format.fprintf oc form in
  let def_var = D.generate_variable ~def_flag:true offset var in
  let val_var = D.generate_variable offset var in
  let locals, set, def, value = D.build_expression se in
  if D.is_always_true def then
    pr "%a%a%a@;@[<v 2>{@;%a@]@;}" D.format_local_declarations locals
      (D.format_set_vars dgfip_flags)
      set
      (D.format_assign dgfip_flags def_var)
      def
      (D.format_assign dgfip_flags val_var)
      value
  else
    pr "%a%a%a@,@[<v 2>if(%s){@;%a@]@,}@,else %s = 0.;"
      D.format_local_declarations locals
      (D.format_set_vars dgfip_flags)
      set
      (D.format_assign dgfip_flags def_var)
      def def_var
      (D.format_assign dgfip_flags val_var)
      value val_var;
  (* If the trace flag is set, we print the value of all non-temp variables *)
  if dgfip_flags.flg_trace && not (Com.Var.is_temp var) then
    pr "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Com.Var.name)
      (VID.gen_pos_from_start var)

let generate_var_def (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (vidx_opt : Mir.expression Pos.marked option)
    (vexpr : Mir.expression Pos.marked) (fmt : Format.formatter) : unit =
  let pr form = Format.fprintf fmt form in
  match vidx_opt with
  | None ->
      let se = generate_c_expr vexpr in
      if Com.Var.is_ref var then (
        pr "@[<v 2>{@;";
        let idx = fresh_c_local "idxPROUT" in
        pr "@;int %s;" idx;
        pr "@;@[<v 2>for(%s = 0; %s < %s; %s++) {" idx idx (VID.gen_size var)
          idx;
        pr "@;%a" (generate_m_assign dgfip_flags var (GetValueExpr idx)) se;
        pr "@]@;}";
        pr "@]@;}@;")
      else generate_m_assign dgfip_flags var None fmt se
  | Some ei ->
      pr "@[<v 2>{@;";
      let idx_val = fresh_c_local "mpp_idx" in
      let idx_def = idx_val ^ "_d" in
      let locals_idx, set_idx, def_idx, value_idx =
        D.build_expression @@ generate_c_expr ei
      in
      pr "char %s;@;long %s;@;%a%a%a@;%a@;" idx_def idx_val
        D.format_local_declarations locals_idx
        (D.format_set_vars dgfip_flags)
        set_idx
        (D.format_assign dgfip_flags idx_def)
        def_idx
        (D.format_assign dgfip_flags idx_val)
        value_idx;
      let size = VID.gen_size var in
      pr "@[<v 2>if(%s && 0 <= %s && %s < %s){@;%a@]@;}" idx_def idx_val idx_val
        size
        (generate_m_assign dgfip_flags var (GetValueExpr idx_val))
        (generate_c_expr vexpr);
      pr "@]@;}@;"

let generate_event_field_def (dgfip_flags : Dgfip_options.flags)
    (idx : Mir.expression Pos.marked) (field : string Pos.marked)
    (expr : Mir.expression Pos.marked) (fmt : Format.formatter) : unit =
  let pr form = Format.fprintf fmt form in
  pr "@[<v 2>{@;";
  let idx_val = fresh_c_local "mpp_idx" in
  let idx_def = idx_val ^ "_d" in
  let locals_idx, set_idx, def_idx, value_idx =
    D.build_expression @@ generate_c_expr idx
  in
  pr "char %s;@;long %s;@;%a%a%a@;%a@;" idx_def idx_val
    D.format_local_declarations locals_idx
    (D.format_set_vars dgfip_flags)
    set_idx
    (D.format_assign dgfip_flags idx_def)
    def_idx
    (D.format_assign dgfip_flags idx_val)
    value_idx;
  pr "@[<v 2>if(%s && 0 <= %s && %s < irdata->nb_events){@;" idx_def idx_val
    idx_val;
  let expr_val = fresh_c_local "mpp_expr" in
  let expr_def = expr_val ^ "_d" in
  let locals_expr, set_expr, def_expr, value_expr =
    D.build_expression @@ generate_c_expr expr
  in
  pr "@[<v 2>{@;char %s;@;double %s;@;%a%a%a@;%a@;" expr_def expr_val
    D.format_local_declarations locals_expr
    (D.format_set_vars dgfip_flags)
    set_expr
    (D.format_assign dgfip_flags expr_def)
    def_expr
    (D.format_assign dgfip_flags expr_val)
    value_expr;
  pr "ecris_varinfo(irdata, irdata->events[%s].field_%s_var, %s, %s);" idx_val
    (Pos.unmark field) expr_def expr_val;
  pr "@]@;}@;";
  pr "@]@;}";
  pr "@]@;}@;"

let rec generate_stmt (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (oc : Format.formatter) (stmt : Mir.m_instruction) =
  match Pos.unmark stmt with
  | Affectation (SingleFormula (VarDecl (m_var, vidx_opt, vexpr)), _) ->
      Format.fprintf oc "@[<v 2>{@;";
      generate_var_def dgfip_flags (Pos.unmark m_var) vidx_opt vexpr oc;
      Format.fprintf oc "@]@;}@;"
  | Affectation (SingleFormula (EventFieldDecl (idx, f, expr)), _) ->
      Format.fprintf oc "@[<v 2>{@;";
      generate_event_field_def dgfip_flags idx f expr oc;
      Format.fprintf oc "@]@;}@;"
  | Affectation (MultipleFormulaes _, _) -> assert false
  | IfThenElse (cond, iftrue, iffalse) ->
      Format.fprintf oc "@[<v 2>{@,";
      let cond_val = fresh_c_local "mpp_cond" in
      let cond_def = cond_val ^ "_d" in
      let locals, set, def, value =
        D.build_expression @@ generate_c_expr cond
      in
      Format.fprintf oc "char %s;@;double %s;@;%a%a%a@;%a@;" cond_def cond_val
        D.format_local_declarations locals
        (D.format_set_vars dgfip_flags)
        set
        (D.format_assign dgfip_flags cond_def)
        def
        (D.format_assign dgfip_flags cond_val)
        value;
      Format.fprintf oc "@[<v 2>if(%s && %s) {@,%a@]@,}" cond_def cond_val
        (generate_stmts dgfip_flags program)
        iftrue;
      if iffalse <> [] then
        Format.fprintf oc "@[<v 2>else if(%s){@,%a@]@,}" cond_def
          (generate_stmts dgfip_flags program)
          iffalse;
      Format.fprintf oc "@]@,}@;"
  | WhenDoElse (wdl, ed) ->
      let pr fmt_str = Format.fprintf oc fmt_str in
      let goto_label = fresh_c_local "when_do_block" in
      let fin_label = fresh_c_local "when_do_end" in
      let cond_val = fresh_c_local "when_do_cond" in
      let cond_def = cond_val ^ "_d" in
      pr "@[<v 2>{@;";
      pr "char %s;@;" cond_def;
      pr "double %s;@;" cond_val;
      let rec aux = function
        | (expr, dl, _) :: l ->
            let locals, set, def, value =
              D.build_expression @@ generate_c_expr expr
            in
            pr "@[<v 2>{@;";
            pr "%a@;" D.format_local_declarations locals;
            pr "%a@;" (D.format_set_vars dgfip_flags) set;
            pr "%a@;" (D.format_assign dgfip_flags cond_def) def;
            pr "%a@;" (D.format_assign dgfip_flags cond_val) value;
            pr "@[<v 2>if(%s) {@;" cond_def;
            pr "if (! %s) goto %s;@;" cond_val goto_label;
            pr "%a@]@;" (generate_stmts dgfip_flags program) dl;
            pr "}@;";
            pr "@]@;}@;";
            aux l
        | [] -> ()
      in
      aux wdl;
      pr "goto %s;@;" fin_label;
      pr "%s:@;" goto_label;
      pr "%a@;" (generate_stmts dgfip_flags program) (Pos.unmark ed);
      pr "%s:{}@]@;" fin_label;
      pr "}@;"
  | VerifBlock stmts ->
      let goto_label = fresh_c_local "verif_block" in
      let pr fmt = Format.fprintf oc fmt in
      pr "@[<v 2>{@;";
      pr "  if (setjmp(irdata->jmp_bloq) != 0) {@;";
      pr "    goto %s;@;" goto_label;
      pr "  }@;";
      pr "%a@;" (generate_stmts dgfip_flags program) stmts;
      pr "%s:;@]@;}" goto_label
  | Print (std, args) ->
      let print_std, pr_ctx =
        match std with
        | StdOut -> ("stdout", "&(irdata->ctx_pr_out)")
        | StdErr -> ("stderr", "&(irdata->ctx_pr_err)")
      in
      let print_val = fresh_c_local "mpp_print" in
      let print_def = print_val ^ "_d" in
      let print_name_or_alias name_or_alias e f =
        let locals, set, def, value = D.build_expression @@ generate_c_expr e in
        Format.fprintf oc "@[<v 2>{%a%a%a@;%a@;@]}@;"
          D.format_local_declarations locals
          (D.format_set_vars dgfip_flags)
          set
          (D.format_assign dgfip_flags print_def)
          def
          (D.format_assign dgfip_flags print_val)
          value;
        Format.fprintf oc "@[<v 2>{@;int idx = (int)floor(%s);@; /* prout */"
          print_val;
        Format.fprintf oc
          "@[<v 2>if(%s && 0 <= idx && idx < irdata->nb_events){@;" print_def;
        Format.fprintf oc
          "print_string(%s, %s, irdata->events[idx].field_%s_var->%s);@]@;"
          print_std pr_ctx (Pos.unmark f) name_or_alias;
        Format.fprintf oc "}@]@;";
        Format.fprintf oc "}@;"
      in
      Format.fprintf oc "@[<v 2>{@,char %s;@;double %s;@;" print_def print_val;
      List.iter
        (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
          match Pos.unmark arg with
          | PrintString s ->
              Format.fprintf oc "print_string(%s, %s, \"%s\");@;" print_std
                pr_ctx (str_escape s)
          | PrintName (var, _) ->
              let ptr = VID.gen_info_ptr var in
              Format.fprintf oc "print_string(%s, %s, %s->name);@;" print_std
                pr_ctx ptr
          | PrintAlias (var, _) ->
              let ptr = VID.gen_info_ptr var in
              Format.fprintf oc "print_string(%s, %s, %s->alias);@;" print_std
                pr_ctx ptr
          | PrintEventName (e, f) -> print_name_or_alias "name" e f
          | PrintEventAlias (e, f) -> print_name_or_alias "alias" e f
          | PrintIndent e ->
              let locals, set, def, value =
                D.build_expression @@ generate_c_expr e
              in
              Format.fprintf oc "@[<v 2>{%a%a%a@;%a@;@]}@;"
                D.format_local_declarations locals
                (D.format_set_vars dgfip_flags)
                set
                (D.format_assign dgfip_flags print_def)
                def
                (D.format_assign dgfip_flags print_val)
                value;
              Format.fprintf oc "@[<v 2>if(%s){@;" print_def;
              Format.fprintf oc "set_print_indent(%s, %s, %s);@]@;" print_std
                pr_ctx print_val;
              Format.fprintf oc "}@;"
          | PrintExpr (e, min, max) ->
              let locals, set, def, value =
                D.build_expression @@ generate_c_expr e
              in
              Format.fprintf oc "@[<v 2>{%a%a%a@;%a@;@]}@;"
                D.format_local_declarations locals
                (D.format_set_vars dgfip_flags)
                set
                (D.format_assign dgfip_flags print_def)
                def
                (D.format_assign dgfip_flags print_val)
                value;
              Format.fprintf oc "@[<v 2>if(%s){@;" print_def;
              Format.fprintf oc "print_double(%s, %s, %s, %d, %d);@]@;"
                print_std pr_ctx print_val min max;
              Format.fprintf oc "@[<v 2>} else {@;";
              Format.fprintf oc "print_string(%s, %s, \"indefini\");@]@;}@;"
                print_std pr_ctx)
        args;
      Format.fprintf oc "@]@;}@;"
  | ComputeTarget ((tn, _), targs) ->
      let pr fmt = Format.fprintf oc fmt in
      ignore
        (List.fold_left
           (fun n ((v : Com.Var.t), _) ->
             let ref_idx = Format.sprintf "irdata->ref_org + %d" n in
             let ref_info = Format.sprintf "irdata->info_ref[%s]" ref_idx in
             let v_info_p = VID.gen_info_ptr v in
             pr "%s = %s;@;" ref_info v_info_p;
             let ref_def = Format.sprintf "irdata->def_ref[%s]" ref_idx in
             let v_def_p = VID.gen_def_ptr v in
             pr "%s = %s;@;" ref_def v_def_p;
             let ref_val = Format.sprintf "irdata->ref[%s]" ref_idx in
             let v_val_p = VID.gen_val_ptr v in
             pr "%s = %s;@;" ref_val v_val_p;
             n + 1)
           0 targs);
      Format.fprintf oc "%s(irdata);" tn
  | Iterate (m_var, vars, var_params, stmts) ->
      let pr fmt = Format.fprintf oc fmt in
      let it_name = fresh_c_local "iterate" in
      let var = Pos.unmark m_var in
      let ref_info = VID.gen_info_ptr var in
      let ref_def = VID.gen_def_ptr var in
      let ref_val = VID.gen_val_ptr var in
      List.iter
        (fun (v, _) ->
          pr "@[<v 2>{@;";
          let v_info_p = VID.gen_info_ptr v in
          pr "%s = %s;@;" ref_info v_info_p;
          let v_def_p = VID.gen_def_ptr v in
          pr "%s = %s;@;" ref_def v_def_p;
          let v_val_p = VID.gen_val_ptr v in
          pr "%s = %s;@;" ref_val v_val_p;
          pr "%a@;" (generate_stmts dgfip_flags program) stmts;
          pr "@]@;}@;")
        vars;
      List.iter
        (fun (vcs, expr) ->
          Com.CatVar.Map.iter
            (fun vc _ ->
              let vcd = Com.CatVar.Map.find vc program.program_var_categories in
              let ref_tab = VID.gen_tab vcd.loc in
              let cond_val = "cond_" ^ it_name in
              let cond_def = cond_val ^ "_d" in
              let locals, set, def, value =
                D.build_expression @@ generate_c_expr expr
              in
              pr "@[<v 2>{@;";
              pr "T_varinfo_%s *tab_%s = varinfo_%s;@;" vcd.id_str it_name
                vcd.id_str;
              pr "int nb_%s = 0;@;" it_name;
              pr "@[<v 2>while (nb_%s < NB_%s) {@;" it_name vcd.id_str;
              pr "char %s;@;" cond_def;
              pr "double %s;@;" cond_val;
              pr "%s = (T_varinfo *)tab_%s;@;" ref_info it_name;
              pr "%s = &(D%s[%s->idx]);@;" ref_def ref_tab ref_info;
              pr "%s = &(%s[%s->idx]);@;" ref_val ref_tab ref_info;
              pr "@[<v 2>{@;";
              pr "%a" D.format_local_declarations locals;
              pr "%a" (D.format_set_vars dgfip_flags) set;
              pr "%a@;" (D.format_assign dgfip_flags cond_def) def;
              pr "%a" (D.format_assign dgfip_flags cond_val) value;
              pr "@]@;";
              pr "}@;";
              pr "@[<hov 2>if(%s && %s){@;" cond_def cond_val;
              pr "%a@]@;" (generate_stmts dgfip_flags program) stmts;
              pr "}@;";
              pr "tab_%s++;@;" it_name;
              pr "nb_%s++;" it_name;
              pr "@]@;}";
              pr "@]@;}@;")
            vcs)
        var_params
  | Iterate_values (m_var, var_intervals, stmts) ->
      let pr fmt = Format.fprintf oc fmt in
      let var = Pos.unmark m_var in
      let itval_def = VID.gen_def var "" in
      let itval_val = VID.gen_val var "" in
      let itval_name = fresh_c_local "iterate_values" in
      let itval_e0_val = Format.sprintf "%s_e0" itval_name in
      let itval_e1_val = Format.sprintf "%s_e1" itval_name in
      let itval_step_val = Format.sprintf "%s_step" itval_name in
      let itval_e0_def = Format.sprintf "%s_def" itval_e0_val in
      let itval_e1_def = Format.sprintf "%s_def" itval_e1_val in
      let itval_step_def = Format.sprintf "%s_def" itval_step_val in
      List.iter
        (fun (e0, e1, step) ->
          let locals_e0, set_e0, def_e0, value_e0 =
            D.build_expression @@ generate_c_expr e0
          in
          let locals_e1, set_e1, def_e1, value_e1 =
            D.build_expression @@ generate_c_expr e1
          in
          let locals_step, set_step, def_step, value_step =
            D.build_expression @@ generate_c_expr step
          in
          pr "@[<v 2>{@;";
          pr "char %s;@;double %s;@;" itval_e0_def itval_e0_val;
          pr "char %s;@;double %s;@;" itval_e1_def itval_e1_val;
          pr "char %s;@;double %s;@;" itval_step_def itval_step_val;
          pr "%a" D.format_local_declarations locals_e0;
          pr "%a" D.format_local_declarations locals_e1;
          pr "%a" D.format_local_declarations locals_step;
          pr "%a" (D.format_set_vars dgfip_flags) set_e0;
          pr "%a" (D.format_set_vars dgfip_flags) set_e1;
          pr "%a" (D.format_set_vars dgfip_flags) set_step;
          pr "%a@;" (D.format_assign dgfip_flags itval_e0_def) def_e0;
          pr "%a@;" (D.format_assign dgfip_flags itval_e1_def) def_e1;
          pr "%a@;" (D.format_assign dgfip_flags itval_step_def) def_step;
          pr "%a@;" (D.format_assign dgfip_flags itval_e0_val) value_e0;
          pr "%a@;" (D.format_assign dgfip_flags itval_e1_val) value_e1;
          pr "%a@;" (D.format_assign dgfip_flags itval_step_val) value_step;
          pr "@[<hov 2>if(%s && %s && %s && %s != 0.0){@;" itval_e0_def
            itval_e1_def itval_step_def itval_step_val;
          pr
            "@[<hov 2>for(%s = 1, %s = %s; (%s > 0.0 ? %s <= %s : %s >= %s); \
             %s = %s + %s){@;"
            itval_def itval_val itval_e0_val itval_step_val itval_val
            itval_e1_val itval_val itval_e1_val itval_val itval_val
            itval_step_val;
          pr "%a@]@;" (generate_stmts dgfip_flags program) stmts;
          pr "}@;";
          pr "@]@;}@;";
          pr "@]@;}")
        var_intervals
  | Restore (vars, var_params, stmts) ->
      let pr fmt = Format.fprintf oc fmt in
      pr "@[<v 2>{@;";
      let rest_name = fresh_c_local "restore" in
      pr "T_env_sauvegarde *%s = NULL;@;" rest_name;
      List.iter
        (fun m_v ->
          let v = Pos.unmark m_v in
          pr "env_sauvegarder(&%s, %s, %s, %s);@;" rest_name (VID.gen_def_ptr v)
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
              let cond_val = "cond_" ^ it_name in
              let cond_def = cond_val ^ "_d" in
              let locals, set, def, value =
                D.build_expression @@ generate_c_expr expr
              in
              pr "@[<v 2>{@;";
              pr "T_varinfo_%s *tab_%s = varinfo_%s;@;" vcd.id_str it_name
                vcd.id_str;
              pr "int nb_%s = 0;@;" it_name;
              pr "@[<v 2>while (nb_%s < NB_%s) {@;" it_name vcd.id_str;
              pr "char %s;@;" cond_def;
              pr "double %s;@;" cond_val;
              pr "%s = (T_varinfo *)tab_%s;@;" ref_info it_name;
              pr "%s = &(D%s[%s->idx]);@;" ref_def ref_tab ref_info;
              pr "%s = &(%s[%s->idx]);@;" ref_val ref_tab ref_info;
              pr "@[<v 2>{@;";
              pr "%a" D.format_local_declarations locals;
              pr "%a" (D.format_set_vars dgfip_flags) set;
              pr "%a@;" (D.format_assign dgfip_flags cond_def) def;
              pr "%a" (D.format_assign dgfip_flags cond_val) value;
              pr "@]@;";
              pr "}@;";
              pr "@[<hov 2>if(%s && %s){@;" cond_def cond_val;
              pr "env_sauvegarder(&%s, %s, %s, %s);" rest_name
                (VID.gen_def_ptr var) (VID.gen_val_ptr var) (VID.gen_size var);
              pr "@]@;";
              pr "}@;";
              pr "tab_%s++;@;" it_name;
              pr "nb_%s++;" it_name;
              pr "@]@;}";
              pr "@]@;}@;")
            vcs)
        var_params;
      pr "%a@;" (generate_stmts dgfip_flags program) stmts;
      pr "env_restaurer(&%s);@;" rest_name;
      pr "@]}@;"
  | RaiseError (m_err, var_opt) ->
      let err = Pos.unmark m_err in
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
    (oc : Format.formatter) (stmts : Mir.m_instruction list) =
  Format.fprintf oc "@[<v>";
  Format.pp_print_list (generate_stmt dgfip_flags program) oc stmts;
  Format.fprintf oc "@]"

let generate_var_tmp_decls (oc : Format.formatter) (tf : Mir.target_data) =
  let pr fmt = Format.fprintf oc fmt in
  if tf.target_sz_tmps > 0 then (
    pr "@[<v 2>{";
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
    pr "@;irdata->ref_org = irdata->ref_org + %d;" tf.target_nb_refs;
  pr "@;"

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
  pr "@[<v 2>%a{@;" (generate_function_prototype false) fd;
  pr "%a@;" generate_var_tmp_decls fd;
  if dgfip_flags.flg_trace then pr "aff1(\"debut %s\\n\");@;" fn;
  pr "%a@;" (generate_stmts dgfip_flags program) fd.target_prog;
  if dgfip_flags.flg_trace then pr "aff1(\"fin %s\\n\");@;" fn;
  pr "@;";
  if fd.target_nb_refs > 0 then
    pr "irdata->ref_org = irdata->ref_org - %d;@;" fd.target_nb_refs;
  if fd.target_sz_tmps > 0 then
    pr "irdata->tmps_org = irdata->tmps_org - %d;@;" fd.target_sz_tmps;
  pr "return 1;@]@;}@\n@\n"

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
  pr "@[<v 2>%a{@;" (generate_target_prototype false) f;
  pr "%a@;" generate_var_tmp_decls tf;
  if dgfip_flags.flg_trace then pr "aff1(\"debut %s\\n\");@;" f;
  pr "%a@;" (generate_stmts dgfip_flags program) tf.target_prog;
  if dgfip_flags.flg_trace then pr "aff1(\"fin %s\\n\");@;" f;
  pr "@;";
  if tf.target_nb_refs > 0 then
    pr "irdata->ref_org = irdata->ref_org - %d;@;" tf.target_nb_refs;
  if tf.target_sz_tmps > 0 then
    pr "irdata->tmps_org = irdata->tmps_org - %d;@;" tf.target_sz_tmps;
  pr "return irdata->discords;@]@;}@\n@\n"

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
              Format.fprintf fmt "#include \"mlang.h\"\n\n";
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
      Format.fprintf fmt "\n@?";
      close_out oc)
    filemap
