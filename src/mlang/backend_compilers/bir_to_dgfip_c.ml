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

let rec lis_tabaccess v m_idx =
  let d_irdata = D.ddirect @@ D.dinstr "irdata" in
  let set_vars, idx_def, idx_val =
    let e_idx = generate_c_expr m_idx in
    (e_idx.set_vars, e_idx.def_test, e_idx.value_comp)
  in
  let res = fresh_c_local "res" in
  let res_def = Pp.spr "%s_def" res in
  let res_val = Pp.spr "%s_val" res in
  let d_fun =
    D.dfun "lis_tabaccess"
      [
        d_irdata;
        D.ddirect @@ D.dinstr @@ Pp.spr "%d" (Com.Var.loc_tab_idx v);
        idx_def;
        idx_val;
        D.ddirect @@ D.dinstr @@ Pp.spr "&%s" res_def;
        D.ddirect @@ D.dinstr @@ Pp.spr "&%s" res_val;
      ]
  in
  let set_vars =
    set_vars
    @ [
        (D.Def, res_def, d_fun); (D.Val, res_val, D.ddirect @@ D.dinstr res_val);
      ]
  in
  let def_test = D.dinstr res_def in
  let value_comp = D.dinstr res_val in
  D.build_transitive_composition { set_vars; def_test; value_comp }

and generate_c_expr (e : Mir.expression Pos.marked) : D.expression_composition =
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
              | Com.VarValue (Pos.Mark (VarAccess v, _)) ->
                  let s_v =
                    let def_test = D.m_var v Def in
                    let value_comp = D.m_var v Val in
                    D.{ set_vars = []; def_test; value_comp }
                  in
                  comparison (Pos.without Com.Eq) sle0 s_v
              | Com.VarValue (Pos.Mark (TabAccess (v, m_i), _)) ->
                  let s_v = lis_tabaccess v m_i in
                  comparison (Pos.without Com.Eq) sle0 s_v
              | Com.VarValue (Pos.Mark (ConcAccess (m_vn, m_if, i), _)) ->
                  let res = fresh_c_local "res" in
                  let res_def = Pp.spr "%s_def" res in
                  let res_val = Pp.spr "%s_val" res in
                  let res_def_ptr = Pp.spr "&%s" res_def in
                  let res_val_ptr = Pp.spr "&%s" res_val in
                  let name = Com.get_normal_var (Pos.unmark m_vn) in
                  let set_vars, def_i, val_i =
                    let ei = generate_c_expr i in
                    (ei.set_vars, ei.def_test, ei.value_comp)
                  in
                  let d_fun =
                    D.dfun "lis_concaccess"
                      [
                        D.ddirect (D.dinstr "irdata");
                        D.ddirect (D.dinstr (Pp.spr "\"%s\"" name));
                        D.ddirect (D.dinstr (Pp.spr "\"%s\"" (Pos.unmark m_if)));
                        def_i;
                        val_i;
                        D.ddirect (D.dinstr res_def_ptr);
                        D.ddirect (D.dinstr res_val_ptr);
                      ]
                  in
                  let set_vars =
                    set_vars
                    @ [
                        (D.Def, res_def, d_fun);
                        (D.Val, res_val, D.ddirect (D.dinstr res_val));
                      ]
                  in
                  let def_test = D.dinstr res_def in
                  let value_comp = D.dinstr res_val in
                  let s_f = D.{ set_vars; def_test; value_comp } in
                  comparison (Pos.without Com.Eq) sle0 s_f
              | Com.VarValue (Pos.Mark (FieldAccess (me, f, _), _)) ->
                  let fn = Pp.spr "event_field_%s" (Pos.unmark f) in
                  let res = fresh_c_local "result" in
                  let res_def = Pp.spr "%s_def" res in
                  let res_val = Pp.spr "%s_val" res in
                  let res_def_ptr = Pp.spr "&%s" res_def in
                  let res_val_ptr = Pp.spr "&%s" res_val in
                  let set_vars, arg_exprs =
                    let e = generate_c_expr me in
                    (e.set_vars, [ e.def_test; e.value_comp ])
                  in
                  let d_fun =
                    D.dfun fn
                      ([
                         D.ddirect (D.dinstr "irdata");
                         D.ddirect (D.dinstr res_def_ptr);
                         D.ddirect (D.dinstr res_val_ptr);
                       ]
                      @ arg_exprs)
                  in
                  let set_vars =
                    set_vars
                    @ [
                        (D.Def, res_def, d_fun);
                        (D.Val, res_val, D.ddirect (D.dinstr res_val));
                      ]
                  in
                  let def_test = D.dinstr res_def in
                  let value_comp = D.dinstr res_val in
                  let s_f = D.{ set_vars; def_test; value_comp } in
                  comparison (Pos.without Com.Eq) sle0 s_f
              | Com.FloatValue i ->
                  let s_i =
                    {
                      D.set_vars = [];
                      D.def_test = D.dtrue;
                      D.value_comp = D.lit (Pos.unmark i);
                    }
                  in
                  comparison (Pos.without Com.Eq) sle0 s_i
              | Com.IntervalValue (bn, en) ->
                  let s_bn =
                    let bn' = float_of_int (Pos.unmark bn) in
                    D.{ set_vars = []; def_test = dtrue; value_comp = lit bn' }
                  in
                  let s_en =
                    let en' = float_of_int (Pos.unmark en) in
                    D.{ set_vars = []; def_test = dtrue; value_comp = lit en' }
                  in
                  binop (Pos.without Com.And)
                    (comparison (Pos.without Com.Gte) sle0 s_bn)
                    (comparison (Pos.without Com.Lte) sle0 s_en)
            in
            binop (Pos.without Com.Or) or_chain equal_test)
          D.{ set_vars = []; def_test = dfalse; value_comp = lit 0. }
          values
      in
      let se = if positive then or_chain else unop Com.Not or_chain in
      {
        D.set_vars = se0.set_vars @ se.set_vars;
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
  | Unop (op, e) -> unop op @@ generate_c_expr e
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
  | FuncCall (Pos.Mark (Supzero, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let cond = D.dand se.def_test (D.comp ">=" se.value_comp (D.lit 0.0)) in
      let def_test = D.ite cond D.dfalse se.def_test in
      let value_comp = D.ite cond (D.lit 0.0) se.value_comp in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (PresentFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = D.dtrue in
      let value_comp = se.def_test in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (NullFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp =
        D.dand def_test (D.comp "==" se.value_comp (D.lit 0.0))
      in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (ArrFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_arr" [ se.value_comp ] in
      (* Here we boldly assume that rounding value of `undef` will give zero,
         given the invariant. Pretty sure that not true, in case of doubt, turn
         `safe_def` to false *)
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (InfFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp = D.dfun "my_floor" [ se.value_comp ] in
      (* same as above *)
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (AbsFunc, _), [ arg ]) ->
      let se = generate_c_expr arg in
      let set_vars = se.D.set_vars in
      let def_test = se.def_test in
      let value_comp = D.dfun "fabs" [ se.value_comp ] in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (MaxFunc, _), [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 in
      let se2 = generate_c_expr e2 in
      let set_vars = se1.D.set_vars @ se2.D.set_vars in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "max" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (MinFunc, _), [ e1; e2 ]) ->
      let se1 = generate_c_expr e1 in
      let se2 = generate_c_expr e2 in
      let set_vars = se1.D.set_vars @ se2.D.set_vars in
      let def_test = D.dor se1.def_test se2.def_test in
      let value_comp = D.dfun "min" [ se1.value_comp; se2.value_comp ] in
      D.build_transitive_composition ~safe_def:true
        { set_vars; def_test; value_comp }
  | FuncCall (Pos.Mark (Multimax, _), [ e1; Pos.Mark (Var m_acc, _) ]) -> (
      match m_acc with
      | VarAccess v ->
          let ptr = VID.gen_info_ptr v in
          let d_irdata = D.ddirect (D.dinstr "irdata") in
          let set_vars, bound_def, bound_val =
            let bound = generate_c_expr e1 in
            (bound.set_vars, bound.def_test, bound.value_comp)
          in
          let res = fresh_c_local "res" in
          let res_def = Pp.spr "%s_def" res in
          let res_val = Pp.spr "%s_val" res in
          let res_def_ptr = Pp.spr "&%s" res_def in
          let res_val_ptr = Pp.spr "&%s" res_val in
          let d_fun =
            D.dfun "multimax_varinfo"
              [
                d_irdata;
                D.ddirect @@ D.dinstr ptr;
                bound_def;
                bound_val;
                D.ddirect @@ D.dinstr res_def_ptr;
                D.ddirect @@ D.dinstr res_val_ptr;
              ]
          in
          let set_vars =
            set_vars
            @ [
                (D.Def, res_def, d_fun);
                (D.Val, res_val, D.ddirect @@ D.dinstr res_val);
              ]
          in
          let def_test = D.dinstr res_def in
          let value_comp = D.dinstr res_val in
          D.build_transitive_composition { set_vars; def_test; value_comp }
      | ConcAccess (m_vn, m_if, i) ->
          let d_irdata = D.ddirect (D.dinstr "irdata") in
          let name = Com.get_normal_var (Pos.unmark m_vn) in
          let set_vars, bound_def, bound_val =
            let bound = generate_c_expr e1 in
            (bound.set_vars, bound.def_test, bound.value_comp)
          in
          let set_vars, conc_d_fun =
            let ei = generate_c_expr i in
            let conc_fn = Pp.spr "lis_concaccess_varinfo" in
            let conc_d_fun =
              D.dfun conc_fn
                [
                  d_irdata;
                  D.ddirect @@ D.dinstr @@ Pp.spr "\"%s\"" name;
                  D.ddirect @@ D.dinstr @@ Pp.spr "\"%s\"" @@ Pos.unmark m_if;
                  ei.def_test;
                  ei.value_comp;
                ]
            in
            (set_vars @ ei.set_vars, conc_d_fun)
          in
          let res = fresh_c_local "res" in
          let res_def = Pp.spr "%s_def" res in
          let res_val = Pp.spr "%s_val" res in
          let res_def_ptr = Pp.spr "&%s" res_def in
          let res_val_ptr = Pp.spr "&%s" res_val in
          let d_fun =
            D.dfun "multimax_varinfo"
              [
                d_irdata;
                D.ddirect conc_d_fun;
                bound_def;
                bound_val;
                D.ddirect (D.dinstr res_def_ptr);
                D.ddirect (D.dinstr res_val_ptr);
              ]
          in
          let set_vars =
            set_vars
            @ [
                (D.Def, res_def, d_fun);
                (D.Val, res_val, D.ddirect (D.dinstr res_val));
              ]
          in
          let def_test = D.dinstr res_def in
          let value_comp = D.dinstr res_val in
          D.build_transitive_composition { set_vars; def_test; value_comp }
      | TabAccess _ | FieldAccess _ -> assert false)
  | FuncCall (Pos.Mark (NbEvents, _), _) ->
      let def_test = D.dinstr "1.0" in
      let value_comp = D.dinstr "nb_evenements(irdata)" in
      D.build_transitive_composition { set_vars = []; def_test; value_comp }
  | FuncCall (Pos.Mark (Func fn, _), args) ->
      let res = fresh_c_local "result" in
      let res_def = Pp.spr "%s_def" res in
      let res_val = Pp.spr "%s_val" res in
      let res_def_ptr = Pp.spr "&%s" res_def in
      let res_val_ptr = Pp.spr "&%s" res_val in
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
             D.ddirect (D.dinstr "irdata");
             D.ddirect (D.dinstr res_def_ptr);
             D.ddirect (D.dinstr res_val_ptr);
           ]
          @ arg_exprs)
      in
      let set_vars =
        set_vars
        @ [
            (D.Def, res_def, d_fun);
            (D.Val, res_val, D.ddirect (D.dinstr res_val));
          ]
      in
      let def_test = D.dinstr res_def in
      let value_comp = D.dinstr res_val in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | FuncCall _ -> assert false (* should not happen *)
  | Literal (Float f) ->
      { set_vars = []; def_test = D.dtrue; value_comp = D.lit f }
  | Literal Undefined ->
      { set_vars = []; def_test = D.dfalse; value_comp = D.lit 0. }
  | Var (VarAccess var) ->
      let def_test = D.m_var var Def in
      let value_comp = D.m_var var Val in
      { set_vars = []; def_test; value_comp }
  | Var (TabAccess (v, m_idx)) -> lis_tabaccess v m_idx
  | Var (ConcAccess (m_vn, m_if, i)) ->
      let d_irdata = D.ddirect (D.dinstr "irdata") in
      let set_vars, i_def, i_val =
        let ei = generate_c_expr i in
        (ei.set_vars, ei.def_test, ei.value_comp)
      in
      let res = fresh_c_local "res" in
      let res_def = Pp.spr "%s_def" res in
      let res_val = Pp.spr "%s_val" res in
      let res_def_ptr = Pp.spr "&%s" res_def in
      let res_val_ptr = Pp.spr "&%s" res_val in
      let name = Com.get_normal_var (Pos.unmark m_vn) in
      let d_fun =
        D.dfun "lis_concaccess"
          [
            d_irdata;
            D.ddirect (D.dinstr (Pp.spr "\"%s\"" name));
            D.ddirect (D.dinstr (Pp.spr "\"%s\"" (Pos.unmark m_if)));
            i_def;
            i_val;
            D.ddirect (D.dinstr res_def_ptr);
            D.ddirect (D.dinstr res_val_ptr);
          ]
      in
      let set_vars =
        set_vars
        @ [
            (D.Def, res_def, d_fun);
            (D.Val, res_val, D.ddirect (D.dinstr res_val));
          ]
      in
      let def_test = D.dinstr res_def in
      let value_comp = D.dinstr res_val in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | Var (FieldAccess (me, f, _)) ->
      let fn = Pp.spr "event_field_%s" (Pos.unmark f) in
      let res = fresh_c_local "result" in
      let res_def = Pp.spr "%s_def" res in
      let res_val = Pp.spr "%s_val" res in
      let res_def_ptr = Pp.spr "&%s" res_def in
      let res_val_ptr = Pp.spr "&%s" res_val in
      let set_vars, arg_exprs =
        let e = generate_c_expr me in
        (e.set_vars, [ e.def_test; e.value_comp ])
      in
      let d_fun =
        D.dfun fn
          ([
             D.ddirect (D.dinstr "irdata");
             D.ddirect (D.dinstr res_def_ptr);
             D.ddirect (D.dinstr res_val_ptr);
           ]
          @ arg_exprs)
      in
      let set_vars =
        set_vars
        @ [
            (D.Def, res_def, d_fun);
            (D.Val, res_val, D.ddirect (D.dinstr res_val));
          ]
      in
      let def_test = D.dinstr res_def in
      let value_comp = D.dinstr res_val in
      D.build_transitive_composition { set_vars; def_test; value_comp }
  | Attribut (m_acc, a) -> (
      let attr = Pos.unmark a in
      match Pos.unmark m_acc with
      | VarAccess v | TabAccess (v, _) ->
          let ptr = VID.gen_info_ptr v in
          let def_test =
            D.dinstr (Pp.spr "attribut_%s_def((T_varinfo *)%s)" attr ptr)
          in
          let value_comp =
            D.dinstr (Pp.spr "attribut_%s((T_varinfo *)%s)" attr ptr)
          in
          D.build_transitive_composition { set_vars = []; def_test; value_comp }
      | ConcAccess (m_vn, m_if, i) ->
          let d_irdata = D.ddirect (D.dinstr "irdata") in
          let name = Com.get_normal_var (Pos.unmark m_vn) in
          let set_vars, conc_d_fun =
            let ei = generate_c_expr i in
            let conc_fn = Pp.spr "lis_concaccess_varinfo" in
            let conc_d_fun =
              D.dfun conc_fn
                [
                  d_irdata;
                  D.ddirect (D.dinstr (Pp.spr "\"%s\"" name));
                  D.ddirect (D.dinstr (Pp.spr "\"%s\"" (Pos.unmark m_if)));
                  ei.def_test;
                  ei.value_comp;
                ]
            in
            (ei.set_vars, conc_d_fun)
          in
          let def_test =
            D.dfun (Pp.spr "attribut_%s_def" attr) [ D.ddirect conc_d_fun ]
          in
          let value_comp =
            D.dfun (Pp.spr "attribut_%s" attr) [ D.ddirect conc_d_fun ]
          in
          D.build_transitive_composition { set_vars; def_test; value_comp }
      | FieldAccess (ie, f, _) ->
          let d_irdata = D.ddirect (D.dinstr "irdata") in
          let set_vars, evt_d_fun =
            let e = generate_c_expr ie in
            let evt_fn = Pp.spr "event_field_%s_var" (Pos.unmark f) in
            (e.set_vars, D.dfun evt_fn [ d_irdata; e.def_test; e.value_comp ])
          in
          let def_test =
            D.dfun (Pp.spr "attribut_%s_def" attr) [ D.ddirect evt_d_fun ]
          in
          let value_comp =
            D.dfun (Pp.spr "attribut_%s" attr) [ D.ddirect evt_d_fun ]
          in
          D.build_transitive_composition { set_vars; def_test; value_comp })
  | Size m_acc -> (
      match Pos.unmark m_acc with
      | VarAccess v ->
          let ptr = VID.gen_info_ptr v in
          let def_test = D.dinstr "1.0" in
          let value_comp = D.dinstr (Format.sprintf "(%s->size)" ptr) in
          D.build_transitive_composition { set_vars = []; def_test; value_comp }
      | TabAccess _ ->
          let def_test = D.dinstr "1.0" in
          let value_comp = D.dinstr "1.0" in
          D.build_transitive_composition { set_vars = []; def_test; value_comp }
      | ConcAccess (m_vn, m_if, i) ->
          let d_irdata = D.ddirect (D.dinstr "irdata") in
          let name = Com.get_normal_var (Pos.unmark m_vn) in
          let set_vars, conc_d_fun =
            let ei = generate_c_expr i in
            let conc_fn = Pp.spr "lis_concaccess_varinfo" in
            let conc_d_fun =
              D.dfun conc_fn
                [
                  d_irdata;
                  D.ddirect (D.dinstr (Pp.spr "\"%s\"" name));
                  D.ddirect (D.dinstr (Pp.spr "\"%s\"" (Pos.unmark m_if)));
                  ei.def_test;
                  ei.value_comp;
                ]
            in
            (ei.set_vars, conc_d_fun)
          in
          let res = fresh_c_local "res" in
          let res_def = Pp.spr "%s_def" res in
          let res_val = Pp.spr "%s_val" res in
          let res_def_ptr = Pp.spr "&%s" res_def in
          let res_val_ptr = Pp.spr "&%s" res_val in
          let d_fun =
            D.dfun "size_varinfo"
              [
                D.ddirect conc_d_fun;
                D.ddirect (D.dinstr res_def_ptr);
                D.ddirect (D.dinstr res_val_ptr);
              ]
          in
          let set_vars =
            set_vars
            @ [
                (D.Def, res_def, d_fun);
                (D.Val, res_val, D.ddirect (D.dinstr res_val));
              ]
          in
          let def_test = D.dinstr res_def in
          let value_comp = D.dinstr res_val in
          D.build_transitive_composition { set_vars; def_test; value_comp }
      | FieldAccess (ie, f, _) ->
          let d_irdata = D.ddirect (D.dinstr "irdata") in
          let set_vars, evt_d_fun =
            let e = generate_c_expr ie in
            let evt_fn = Pp.spr "event_field_%s_var" (Pos.unmark f) in
            (e.set_vars, D.dfun evt_fn [ d_irdata; e.def_test; e.value_comp ])
          in
          let res = fresh_c_local "res" in
          let res_def = Pp.spr "%s_def" res in
          let res_val = Pp.spr "%s_val" res in
          let res_def_ptr = Pp.spr "&%s" res_def in
          let res_val_ptr = Pp.spr "&%s" res_val in
          let d_fun =
            D.dfun "size_varinfo"
              [
                D.ddirect evt_d_fun;
                D.ddirect (D.dinstr res_def_ptr);
                D.ddirect (D.dinstr res_val_ptr);
              ]
          in
          let set_vars =
            set_vars
            @ [
                (D.Def, res_def, d_fun);
                (D.Val, res_val, D.ddirect (D.dinstr res_val));
              ]
          in
          let def_test = D.dinstr res_def in
          let value_comp = D.dinstr res_val in
          D.build_transitive_composition { set_vars; def_test; value_comp })
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
    pr "@;@[<v 2>{%a%a%a%a@]@;}" D.format_local_declarations locals
      (D.format_set_vars dgfip_flags)
      set
      (D.format_assign dgfip_flags res_def)
      def
      (D.format_assign dgfip_flags res_val)
      value
  else
    pr "@;@[<v 2>{%a%a%a@;@[<v 2>if (%s) {%a@]@;} else %s = 0.0;@]@;}"
      D.format_local_declarations locals
      (D.format_set_vars dgfip_flags)
      set
      (D.format_assign dgfip_flags res_def)
      def res_def
      (D.format_assign dgfip_flags res_val)
      value res_val

let generate_m_assign (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (oc : Format.formatter) (expr : Mir.expression Pos.marked) : unit =
  let var_def = D.generate_variable ~def_flag:true var in
  let var_val = D.generate_variable var in
  generate_expr_with_res_in dgfip_flags oc var_def var_val expr;
  (* If the trace flag is set, we print the value of all non-temp variables *)
  if dgfip_flags.flg_trace && not (Com.Var.is_temp var) then
    Format.fprintf oc "@;aff2(\"%s\", irdata, %s);"
      (Pos.unmark var.Com.Var.name)
      (VID.gen_pos_from_start var)

let generate_var_def (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (vexpr : Mir.expression Pos.marked) (oc : Format.formatter) : unit =
  generate_m_assign dgfip_flags var oc vexpr

let generate_var_def_tab (dgfip_flags : Dgfip_options.flags) (var : Com.Var.t)
    (vidx : Mir.m_expression) (vexpr : Mir.m_expression) (oc : Format.formatter)
    : unit =
  let pr form = Format.fprintf oc form in
  pr "@;@[<v 2>{";
  let idx_tab = Com.Var.loc_tab_idx var in
  pr "@;T_varinfo *info = tab_varinfo[%d];" idx_tab;
  let idx = fresh_c_local "idx" in
  let idx_def = idx ^ "_def" in
  let idx_val = idx ^ "_val" in
  pr "@;char %s;@;double %s;@;int %s;" idx_def idx_val idx;
  generate_expr_with_res_in dgfip_flags oc idx_def idx_val vidx;
  pr "@;%s = (int)%s;" idx idx_val;
  pr "@;@[<v 2>if (%s && 0 <= %s && %s < info->size) {" idx_def idx idx;
  let res = fresh_c_local "res" in
  let res_def = res ^ "_def" in
  let res_val = res ^ "_val" in
  pr "@;char %s;@;double %s;" res_def res_val;
  generate_expr_with_res_in dgfip_flags oc res_def res_val vexpr;
  pr "@;ecris_tabaccess(irdata, %d, %s, %s, %s, %s);" idx_tab idx_def idx_val
    res_def res_val;
  pr "@]@;}";
  pr "@]@;}"

let generate_conc_def (dgfip_flags : Dgfip_options.flags)
    (m_vn : Com.m_var_name) (m_if : string Pos.marked)
    (idx_expr : Mir.m_expression) (expr : Mir.m_expression)
    (oc : Format.formatter) : unit =
  let pr form = Format.fprintf oc form in
  pr "@;@[<v 2>{";
  let idx = fresh_c_local "idx" in
  let idx_def = idx ^ "_def" in
  let idx_val = idx ^ "_val" in
  pr "@;char %s;@;double %s;" idx_def idx_val;
  let res = fresh_c_local "res" in
  let res_def = res ^ "_def" in
  let res_val = res ^ "_val" in
  pr "@;char %s;@;double %s;" res_def res_val;
  generate_expr_with_res_in dgfip_flags oc idx_def idx_val idx_expr;
  generate_expr_with_res_in dgfip_flags oc res_def res_val expr;
  let name = Com.get_normal_var (Pos.unmark m_vn) in
  pr "@;ecris_concaccess(irdata, \"%s\", \"%s\", %s, %s, %s, %s);" name
    (Pos.unmark m_if) idx_def idx_val res_def res_val;
  pr "@]@;}"

let generate_event_field_def (dgfip_flags : Dgfip_options.flags)
    (p : Mir.program) (idx_expr : Mir.expression Pos.marked) (field : string)
    (vidx_opt : Mir.expression Pos.marked option)
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
  if (StrMap.find field p.program_event_fields).is_var then (
    match vidx_opt with
    | None ->
        pr "@;ecris_varinfo(irdata, irdata->events[%s]->field_%s_var, %s, %s);"
          idx field res_def res_val
    | Some ei ->
        let i = fresh_c_local "i" in
        let i_def = i ^ "_def" in
        let i_val = i ^ "_val" in
        pr "@;char %s;@;double %s;@;int %s;" i_def i_val i;
        generate_expr_with_res_in dgfip_flags oc i_def i_val ei;
        pr "@;%s = (int)%s;" i i_val;
        pr
          "@;\
           ecris_varinfo_tab(irdata, irdata->events[%s]->field_%s_var, %s, %s, \
           %s);"
          idx i field res_def res_val)
  else (
    pr "@;irdata->events[%s]->field_%s_def = %s;" idx field res_def;
    pr "@;irdata->events[%s]->field_%s_val = %s;" idx field res_val);
  pr "@]@;}";
  pr "@]@;}"

let generate_event_field_ref (dgfip_flags : Dgfip_options.flags)
    (p : Mir.program) (idx_expr : Mir.expression Pos.marked) (field : string)
    (var : Com.Var.t) (oc : Format.formatter) : unit =
  if (StrMap.find field p.program_event_fields).is_var then (
    let pr form = Format.fprintf oc form in
    let idx = fresh_c_local "idx" in
    let idx_def = idx ^ "_def" in
    let idx_val = idx ^ "_val" in
    let var_info_ptr = VID.gen_info_ptr var in
    pr "@;@[<v 2>{";
    pr "@;char %s;@;double %s;@;int %s;" idx_def idx_val idx;
    generate_expr_with_res_in dgfip_flags oc idx_def idx_val idx_expr;
    pr "@;%s = (int)%s;" idx idx_val;
    pr "@;@[<v 2>if (%s && 0 <= %s && %s < irdata->nb_events) {" idx_def idx idx;
    pr "@;irdata->events[%s]->field_%s_var = %s;" idx field var_info_ptr;
    pr "@]@;}";
    pr "@]@;}")

let rec generate_stmt (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (oc : Format.formatter) (stmt : Mir.m_instruction) =
  let pr fmt = Format.fprintf oc fmt in
  match Pos.unmark stmt with
  | Affectation (Pos.Mark (SingleFormula (VarDecl (m_acc, expr)), _)) -> (
      match Pos.unmark m_acc with
      | VarAccess v -> generate_var_def dgfip_flags v expr oc
      | TabAccess (v, m_idx) -> generate_var_def_tab dgfip_flags v m_idx expr oc
      | ConcAccess (m_vn, m_if, i) ->
          generate_conc_def dgfip_flags m_vn m_if i expr oc
      | FieldAccess (i, f, _) ->
          let fn = Pos.unmark f in
          generate_event_field_def dgfip_flags program i fn None expr oc)
  | Affectation (Pos.Mark (SingleFormula (EventFieldRef (idx, f, _, var)), _))
    ->
      generate_event_field_ref dgfip_flags program idx (Pos.unmark f) var oc
  | Affectation (Pos.Mark (MultipleFormulaes _, _)) -> assert false
  | IfThenElse (cond_expr, iftrue, iffalse) ->
      pr "@;@[<v 2>{";
      let cond = fresh_c_local "cond" in
      let cond_def = cond ^ "_def" in
      let cond_val = cond ^ "_val" in
      pr "@;char %s;@;double %s;" cond_def cond_val;
      generate_expr_with_res_in dgfip_flags oc cond_def cond_val cond_expr;
      pr "@;@[<v 2>if (%s && %s != 0.0) {" cond_def cond_val;
      pr "%a" (generate_stmts dgfip_flags program) iftrue;
      if iffalse <> [] then (
        pr "@]@;@[<v 2>} else if (%s) {" cond_def;
        pr "%a" (generate_stmts dgfip_flags program) iffalse);
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
            pr "%a" (generate_stmts dgfip_flags program) dl;
            pr "@]@;}";
            aux l
        | [] -> ()
      in
      aux wdl;
      pr "@;goto %s;" fin_label;
      pr "@;%s:" goto_label;
      pr "%a" (generate_stmts dgfip_flags program) (Pos.unmark ed);
      pr "@;%s:{}" fin_label;
      pr "@]@;}"
  | VerifBlock stmts ->
      let goto_label = fresh_c_local "verif_block" in
      pr "@;@[<v 2>{";
      pr "@;if (setjmp(irdata->jmp_bloq) != 0) goto %s;" goto_label;
      pr "%a" (generate_stmts dgfip_flags program) stmts;
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
      List.iter
        (fun (arg : Com.Var.t Com.print_arg Pos.marked) ->
          match Pos.unmark arg with
          | PrintString s ->
              pr "@;print_string(%s, %s, \"%s\");" print_std pr_ctx
                (str_escape s)
          | PrintAccess (info, m_a) -> (
              match Pos.unmark m_a with
              | VarAccess v ->
                  let ptr = VID.gen_info_ptr v in
                  let fld =
                    match info with Com.Name -> "name" | Com.Alias -> "alias"
                  in
                  pr "@;print_string(%s, %s, %s->%s);" print_std pr_ctx ptr fld
              | TabAccess (v, m_idx) ->
                  pr "@;@[<v 2>{";
                  pr "T_varinfo *info;";
                  let idx_tab = Com.Var.loc_tab_idx v in
                  generate_expr_with_res_in dgfip_flags oc print_def print_val
                    m_idx;
                  pr "info = lis_tabaccess_varinfo(irdata, %d, %s, %s);" idx_tab
                    print_def print_val;
                  let fld =
                    match info with Com.Name -> "name" | Com.Alias -> "alias"
                  in
                  pr "@;print_string(%s, %s, (info == NULL ? \"\" : info->%s));"
                    print_std pr_ctx fld;
                  pr "@]@;}"
              | ConcAccess (m_vn, m_if, i) ->
                  let fld =
                    match info with Com.Name -> "name" | Com.Alias -> "alias"
                  in
                  let name = Com.get_normal_var (Pos.unmark m_vn) in
                  generate_expr_with_res_in dgfip_flags oc print_def print_val i;
                  pr "@;%s = (int)%s;" print print_val;
                  pr "@;@[<v 2>{";
                  pr
                    "@;\
                     T_varinfo *info = lis_concaccess_varinfo(irdata, \"%s\", \
                     \"%s\", %s, %s);"
                    name (Pos.unmark m_if) print_def print_val;
                  pr "@;@[<v 2>if (info != NULL) {";
                  pr "@;print_string(%s, %s, info->%s);" print_std pr_ctx fld;
                  pr "@]@;}";
                  pr "@]@;}"
              | FieldAccess (e, f, _) ->
                  let fld =
                    match info with Com.Name -> "name" | Com.Alias -> "alias"
                  in
                  let ef =
                    StrMap.find (Pos.unmark f) program.program_event_fields
                  in
                  if ef.is_var then (
                    generate_expr_with_res_in dgfip_flags oc print_def print_val
                      e;
                    pr "@;%s = (int)%s;" print print_val;
                    pr "@;@[<v 2>if (%s && 0 <= %s && %s < irdata->nb_events) {"
                      print_def print print;
                    pr
                      "@;\
                       print_string(%s, %s, \
                       irdata->events[%s]->field_%s_var->%s);"
                      print_std pr_ctx print (Pos.unmark f) fld;
                    pr "@]@;}"))
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
  | ComputeTarget (Pos.Mark (tn, _), targs) ->
      ignore
        (List.fold_left
           (fun n (v : Com.Var.t) ->
             let ref_idx = Format.sprintf "irdata->ref_org + %d" n in
             let ref_name = Format.sprintf "irdata->ref_name[%s]" ref_idx in
             pr "@;%s = \"%s\";" ref_name (Com.Var.name_str v);
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
  | Iterate (var, vars, var_params, stmts) ->
      let it_name = fresh_c_local "iterate" in
      let ref_name = VID.gen_ref_name_ptr var in
      let ref_info = VID.gen_info_ptr var in
      let ref_def = VID.gen_def_ptr var in
      let ref_val = VID.gen_val_ptr var in
      pr "@;%s = \"%s\";" ref_name (Com.Var.name_str var);
      List.iter
        (fun v ->
          pr "@;@[<v 2>{";
          pr "@;%s = %s;" ref_info (VID.gen_info_ptr v);
          pr "@;%s = %s;" ref_def (VID.gen_def_ptr v);
          pr "@;%s = %s;" ref_val (VID.gen_val_ptr v);
          pr "%a" (generate_stmts dgfip_flags program) stmts;
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
              pr "@;@[<hov 2>if (%s->tab_idx < 0) {" ref_info;
              pr "@;%s = &(D%s[%s->idx]);" ref_def ref_tab ref_info;
              pr "@;%s = &(%s[%s->idx]);" ref_val ref_tab ref_info;
              generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
              pr "@;@[<hov 2>if (%s && %s != 0.0) {" cond_def cond_val;
              pr "%a" (generate_stmts dgfip_flags program) stmts;
              pr "@]@;}";
              pr "@]@;}";
              pr "@;tab_%s++;" it_name;
              pr "@;nb_%s++;" it_name;
              pr "@]@;}";
              pr "@]@;}")
            vcs)
        var_params
  | Iterate_values (var, var_intervals, stmts) ->
      let itval_def = VID.gen_def var in
      let itval_val = VID.gen_val var in
      let postfix = fresh_c_local "" in
      let e0_def = Format.sprintf "e0_def%s" postfix in
      let e0_val = Format.sprintf "e0_val%s" postfix in
      let e1_def = Format.sprintf "e1_def%s" postfix in
      let e1_val = Format.sprintf "e1_val%s" postfix in
      let step_def = Format.sprintf "step_def%s" postfix in
      let step_val = Format.sprintf "step_val%s" postfix in
      List.iter
        (fun (e0, e1, step) ->
          pr "@;@[<v 2>{";
          pr "@;char %s;@;double %s;" e0_def e0_val;
          pr "@;char %s;@;double %s;" e1_def e1_val;
          pr "@;char %s;@;double %s;" step_def step_val;
          generate_expr_with_res_in dgfip_flags oc e0_def e0_val e0;
          generate_expr_with_res_in dgfip_flags oc e1_def e1_val e1;
          generate_expr_with_res_in dgfip_flags oc step_def step_val step;
          pr "@;@[<v 2>if(%s && %s && %s && %s != 0.0) {" e0_def e1_def step_def
            step_val;
          pr
            "@;\
             @[<v 2>@[<hov 2>for (%s = 1,@ %s = %s;@ (%s > 0.0 ? %s <= %s : %s \
             >= %s);@ %s = %s + %s) {@]"
            itval_def itval_val e0_val step_val itval_val e1_val itval_val
            e1_val itval_val itval_val step_val;
          pr "%a" (generate_stmts dgfip_flags program) stmts;
          pr "@]@;}";
          pr "@]@;}";
          pr "@]@;}")
        var_intervals
  | ArrangeEvents (sort, filter, add, stmts) ->
      let events_sav = fresh_c_local "events_sav" in
      let events_tmp = fresh_c_local "events_tmp" in
      let nb_events_sav = fresh_c_local "nb_events_sav" in
      let nb_add = fresh_c_local "nb_add" in
      let cpt_i = fresh_c_local "i" in
      let cpt_j = fresh_c_local "j" in
      let evt = fresh_c_local "evt" in
      pr "@;@[<v 2>{";
      pr "@;T_event **%s = irdata->events;" events_sav;
      pr "@;int %s = irdata->nb_events;" nb_events_sav;
      pr "@;int %s = 0;" nb_add;
      pr "@;T_event **%s = NULL;" events_tmp;
      pr "@;int %s = 0;" cpt_i;
      pr "@;int %s = 0;" cpt_j;
      (match add with
      | Some expr ->
          pr "@;@[<v 2>{";
          let cond = fresh_c_local "cond" in
          let cond_def = cond ^ "_def" in
          let cond_val = cond ^ "_val" in
          pr "@;char %s;@;double %s;" cond_def cond_val;
          generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
          pr "@;%s = (int)%s;" nb_add cond_val;
          pr "@;if (%s < 0) %s = 0;" nb_add nb_add;
          pr "@;@[<v 2>if (%s && 0 < %s) {" cond_def nb_add;
          let cpt_k = fresh_c_local "k" in
          pr "@;int %s = 0;" cpt_k;
          pr "@;%s = (T_event **)malloc((%s + %s) * (sizeof (T_event *)));"
            events_tmp nb_events_sav nb_add;
          pr "@;@[<v 2>for (%s = 0; %s < %s; %s++) {" cpt_k cpt_k nb_add cpt_k;
          pr "@;T_event *%s = (T_event *)malloc(sizeof (T_event));" evt;
          StrMap.iter
            (fun f (ef : Com.event_field) ->
              if ef.is_var then
                let _, var = StrMap.min_binding program.program_vars in
                pr "@;%s->field_%s_var = %s;" evt f (VID.gen_info_ptr var)
              else (
                pr "@;%s->field_%s_def = 0;" evt f;
                pr "@;%s->field_%s_val = 0.0;" evt f))
            program.program_event_fields;
          pr "@;%s[%s] = %s;" events_tmp cpt_k evt;
          pr "@]@;}";
          pr "@]@;@[<v 2>} else {";
          pr "@;%s = 0;" nb_add;
          pr "@;%s = (T_event **)malloc(%s * (sizeof (T_event *)));" events_tmp
            nb_events_sav;
          pr "@]@;}";
          pr "@;%s = %s;" cpt_i nb_add;
          pr "@]@;}"
      | None ->
          pr "@;%s = (T_event **)malloc(%s * (sizeof (T_event *)));" events_tmp
            nb_events_sav);
      (match filter with
      | Some (var, expr) ->
          pr "@;@[<v 2>while(%s < %s) {" cpt_j nb_events_sav;
          let ref_def = VID.gen_def var in
          let ref_val = VID.gen_val var in
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
          pr "@;@[<v 2>while (%s < %s) {" cpt_i nb_events_sav;
          pr "@;%s[%s] = irdata->events[%s];" events_tmp cpt_i cpt_i;
          pr "@;%s++;" cpt_i;
          pr "@]@;}";
          pr "@;irdata->events = %s;" events_tmp;
          pr "@;irdata->nb_events = %s;" cpt_i);
      (match sort with
      | Some (var0, var1, expr) ->
          pr "@;/* merge sort */";
          pr "@;@[<v 2>{";
          pr "@;int aBeg = %s;" nb_add;
          pr "@;int aEnd = irdata->nb_events;";
          pr
            "@;\
             T_event **b = (T_event **)malloc(irdata->nb_events * (sizeof \
             (T_event *)));";
          pr "@;int width;";
          pr "@;int iLeft;";
          pr "@;int i;";
          pr
            "@;\
             @[<v 2>@[<hov 2>for (width = 1;@ width < aEnd;@ width = 2 * \
             width) {@]";
          pr
            "@;\
             @[<v 2>@[<hov 2>for (iLeft = aBeg;@ iLeft < aEnd;@ iLeft = iLeft \
             + 2 * width) {@]";
          pr "@;int iRight = iLeft + width;";
          pr "@;int iEnd = iLeft + 2 * width;";
          pr "@;if (iRight > aEnd) iRight = aEnd;";
          pr "@;if (iEnd > aEnd) iEnd = aEnd;";
          pr "@;@[<v 2>{";
          pr "@;int i = iLeft;";
          pr "@;int j = iRight;";
          pr "@;int k;";
          pr "@;@[<v 2>@[<hov 2>for (k = iLeft;@ k < iEnd;@ k++) {@]";
          pr "@;int cpt = 0;";
          pr "@;@[<v 2>{";
          (* Comparaison *)
          let ref0_def = VID.gen_def var0 in
          let ref0_val = VID.gen_val var0 in
          let ref1_def = VID.gen_def var1 in
          let ref1_val = VID.gen_val var1 in
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
          pr "@;@[<v 2>@[<hov 2>for (i = aBeg;@ i < aEnd;@ i++) {@]";
          pr "@;irdata->events[i] = b[i];";
          pr "@]@;}";
          pr "@]@;}";
          pr "@;free(b);";
          pr "@]@;}"
      | None -> ());
      pr "%a" (generate_stmts dgfip_flags program) stmts;
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
        (fun v ->
          pr "@;env_sauvegarder(&%s, %s, %s, %s);" rest_name (VID.gen_def_ptr v)
            (VID.gen_val_ptr v) (VID.gen_size v))
        vars;
      List.iter
        (fun (var, vcs, expr) ->
          let it_name = fresh_c_local "iterate" in
          Com.CatVar.Map.iter
            (fun vc _ ->
              let vcd = Com.CatVar.Map.find vc program.program_var_categories in
              let ref_tab = VID.gen_tab vcd.loc in
              let ref_name = VID.gen_ref_name_ptr var in
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
              pr "@;%s = \"%s\";" ref_name (Com.Var.name_str var);
              pr "@;@[<v 2>while (nb_%s < NB_%s) {" it_name vcd.id_str;
              pr "@;char %s;@;double %s;" cond_def cond_val;
              pr "@;%s = (T_varinfo *)tab_%s;" ref_info it_name;
              pr "@;%s = &(D%s[%s->idx]);" ref_def ref_tab ref_info;
              pr "@;%s = &(%s[%s->idx]);" ref_val ref_tab ref_info;
              generate_expr_with_res_in dgfip_flags oc cond_def cond_val expr;
              pr "@;@[<v 2>if (%s && %s != 0.0) {" cond_def cond_val;
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
        (fun (var, expr) ->
          let idx = fresh_c_local "idx" in
          let ref_def = VID.gen_def var in
          let ref_val = VID.gen_val var in
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
          pr "@;@[<v 2>if (%s && %s != 0.0) {" cond_def cond_val;
          pr "@;env_sauvegarder_evt(&%s, irdata->events[%s]);@;" rest_evt_name
            idx;
          pr "@]@;}";
          pr "@;%s++;" idx;
          pr "@]@;}";
          pr "@]@;}")
        evtfs;
      pr "%a" (generate_stmts dgfip_flags program) stmts;
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
  List.iter (generate_stmt dgfip_flags program oc) stmts

let generate_function_tmp_decls (oc : Format.formatter) (tf : Mir.target) =
  let pr fmt = Format.fprintf oc fmt in
  let nb_args = List.length tf.target_args in
  pr "@;@[<v 2>{";
  pr "@;int i;";
  pr "@;T_varinfo *info;";
  pr "@;irdata->def_tmps[irdata->tmps_org] = 0;";
  pr "@;irdata->tmps[irdata->tmps_org] = 0.0;";
  pr "@;irdata->info_tmps[irdata->tmps_org] = NULL;";
  List.iteri
    (fun i var ->
      let idx = Pp.spr "irdata->tmps_org + %d" (i + 1) in
      pr "@;irdata->def_tmps[%s] = arg_def%d;" idx i;
      pr "@;irdata->tmps[%s] = arg_val%d;" idx i;
      let loc_cat_idx = Com.Var.loc_cat_idx var in
      let name = Com.Var.name_str var in
      pr "@;irdata->info_tmps[%s] = &(tmp_varinfo[%d]); /* %s  */" idx
        loc_cat_idx name)
    tf.target_args;
  let vres = Option.get tf.target_result in
  let loc_cat_idx_vres = Com.Var.loc_cat_idx vres in
  let name_vres = Com.Var.name_str vres in
  pr "@;irdata->def_tmps[irdata->tmps_org] = 0;";
  pr "@;irdata->tmps[irdata->tmps_org] = 0.0;";
  pr "@;irdata->info_tmps[irdata->tmps_org] = &(tmp_varinfo[%d]); /* %s */"
    loc_cat_idx_vres name_vres;
  if tf.target_sz_tmps > 0 then (
    pr "@;@[<v 2>@[<hov 2>for (i = %d;@ i < %d;@ i++) {@]" (1 + nb_args)
      tf.target_sz_tmps;
    pr "@;irdata->def_tmps[irdata->tmps_org + i] = 0;";
    pr "@;irdata->tmps[irdata->tmps_org + i] = 0.0;";
    pr "@;irdata->info_tmps[irdata->tmps_org + i] = NULL;";
    pr "@]@;}";
    pr "@;irdata->tmps_org = irdata->tmps_org + %d;" tf.target_sz_tmps;
    StrMap.iter
      (fun vn var ->
        let loc_str = Pp.spr "irdata->tmps_org + (%d)" (Com.Var.loc_idx var) in
        let loc_cat_idx = Com.Var.loc_cat_idx var in
        pr "@;irdata->info_tmps[%s] = &(tmp_varinfo[%d]); /* %s */" loc_str
          loc_cat_idx vn)
      tf.target_tmp_vars);
  pr "@]@;}";
  if tf.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org + %d;" tf.target_nb_refs

let generate_function_prototype (add_semicolon : bool) (oc : Format.formatter)
    (fd : Mir.target) =
  let fn = Pos.unmark fd.target_name in
  let pp_args fmt args =
    List.iteri
      (fun i _ -> Pp.fpr fmt ", char arg_def%d, double arg_val%d" i i)
      args
  in
  Format.fprintf oc
    "int %s(T_irdata* irdata, char *res_def, double *res_val%a)%s" fn pp_args
    fd.target_args
    (if add_semicolon then ";" else "")

let generate_function (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program) (oc : Format.formatter) (fn : string) =
  let pr fmt = Format.fprintf oc fmt in
  let fd = StrMap.find fn program.program_functions in
  pr "@.@[<v 2>%a {" (generate_function_prototype false) fd;
  let sav = fresh_c_local "sav" in
  let sav_nb_tmps = Pp.spr "%s_nb_tmps_target" sav in
  let sav_nb_refs = Pp.spr "%s_nb_refs_target" sav in
  pr "@;int %s = irdata->nb_tmps_target;" sav_nb_tmps;
  pr "@;int %s = irdata->nb_refs_target;" sav_nb_refs;
  pr "%a" generate_function_tmp_decls fd;
  pr "@;irdata->nb_tmps_target = %d;"
    (StrMap.fold (fun _ v n -> n + Com.Var.size v) fd.target_tmp_vars 0);
  pr "@;irdata->nb_refs_target = %d;" fd.target_nb_refs;
  pr "@;";
  if dgfip_flags.flg_trace then pr "@;aff1(\"debut %s\\n\");" fn;
  pr "%a" (generate_stmts dgfip_flags program) fd.target_prog;

  if dgfip_flags.flg_trace then pr "@;aff1(\"fin %s\\n\");" fn;
  pr "@;";
  if fd.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org - %d;" fd.target_nb_refs;
  if fd.target_sz_tmps > 0 then
    pr "@;irdata->tmps_org = irdata->tmps_org - %d;" fd.target_sz_tmps;
  pr "@;irdata->nb_refs_target = %s;" sav_nb_refs;
  pr "@;irdata->nb_tmps_target = %s;" sav_nb_tmps;
  pr "@;*res_def = irdata->def_tmps[irdata->tmps_org];";
  pr "@;*res_val = irdata->tmps[irdata->tmps_org];";
  pr "@;return 1;";
  pr "@]@;}@."

let generate_functions (dgfip_flags : Dgfip_options.flags)
    (program : Mir.program)
    (filemap : (out_channel * Format.formatter) StrMap.t) =
  let functions = StrMap.bindings program.program_functions in
  List.iter
    (fun (name, ({ target_file; _ } : Mir.target)) ->
      let file_str = match target_file with Some s -> s | None -> "" in
      let _, fmt = StrMap.find file_str filemap in
      generate_function (dgfip_flags : Dgfip_options.flags) program fmt name)
    functions

let generate_target_prototype (add_semicolon : bool) (oc : Format.formatter)
    (function_name : string) =
  Format.fprintf oc "struct S_discord * %s(T_irdata* irdata)%s" function_name
    (if add_semicolon then ";" else "")

let generate_cible_tmp_decls (oc : Format.formatter) (tf : Mir.target) =
  let pr fmt = Format.fprintf oc fmt in
  if tf.target_sz_tmps > 0 then (
    pr "@;@[<v 2>{";
    pr "@;int i;";
    pr "@;T_varinfo *info;";
    pr "@;@[<v 2>@[<hov 2>for (i = 0;@ i < %d;@ i++) {@]" tf.target_sz_tmps;
    pr "@;irdata->def_tmps[irdata->tmps_org + i] = 0;";
    pr "@;irdata->tmps[irdata->tmps_org + i] = 0.0;";
    pr "@;irdata->info_tmps[irdata->tmps_org + i] = NULL;";
    pr "@]@;}";
    pr "@;irdata->tmps_org = irdata->tmps_org + %d;" tf.target_sz_tmps;
    StrMap.iter
      (fun vn var ->
        let loc_str = Pp.spr "irdata->tmps_org + (%d)" (Com.Var.loc_idx var) in
        let loc_cat_idx = Com.Var.loc_cat_idx var in
        pr "@;irdata->info_tmps[%s] = &(tmp_varinfo[%d]); /* %s */" loc_str
          loc_cat_idx vn)
      tf.target_tmp_vars;
    pr "@]@;}");
  if tf.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org + %d;" tf.target_nb_refs

let generate_target (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (oc : Format.formatter) (f : string) =
  let pr fmt = Format.fprintf oc fmt in
  let tf = StrMap.find f program.program_targets in
  pr "@.@[<v 2>%a {" (generate_target_prototype false) f;
  let sav = fresh_c_local "sav" in
  let sav_nb_tmps = Pp.spr "%s_nb_tmps_target" sav in
  let sav_nb_refs = Pp.spr "%s_nb_refs_target" sav in
  pr "@;int %s = irdata->nb_tmps_target;" sav_nb_tmps;
  pr "@;int %s = irdata->nb_refs_target;" sav_nb_refs;
  pr "%a" generate_cible_tmp_decls tf;
  pr "@;irdata->nb_tmps_target = %d;"
    (StrMap.fold (fun _ v n -> n + Com.Var.size v) tf.target_tmp_vars 0);
  pr "@;irdata->nb_refs_target = %d;" tf.target_nb_refs;
  pr "@;";
  if dgfip_flags.flg_trace then pr "@;aff1(\"debut %s\\n\");" f;
  pr "%a" (generate_stmts dgfip_flags program) tf.target_prog;
  if dgfip_flags.flg_trace then pr "@;aff1(\"fin %s\\n\");" f;
  pr "@;";
  if tf.target_nb_refs > 0 then
    pr "@;irdata->ref_org = irdata->ref_org - %d;" tf.target_nb_refs;
  if tf.target_sz_tmps > 0 then
    pr "@;irdata->tmps_org = irdata->tmps_org - %d;" tf.target_sz_tmps;
  pr "@;irdata->nb_refs_target = %s;" sav_nb_refs;
  pr "@;irdata->nb_tmps_target = %s;" sav_nb_tmps;
  pr "@;return irdata->discords;";
  pr "@]@;}@."

let generate_targets (dgfip_flags : Dgfip_options.flags) (program : Mir.program)
    (filemap : (out_channel * Format.formatter) StrMap.t) =
  let targets = StrMap.bindings program.program_targets in
  List.iter
    (fun (name, ({ target_file; _ } : Mir.target)) ->
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
    StrMap.fold
      (fun _ (t : Mir.target) filemap ->
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
