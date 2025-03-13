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

(** {!module: Mast} to {!module: Mir} translation of M programs. *)

let complete_vars_stack (prog : Validator.program) : Validator.program =
  let prog_functions, prog_targets =
    let rec aux_instrs mil =
      let fold (nbRef, nbIt) mi =
        let nbRef', nbIt' = aux_instr mi in
        (max nbRef nbRef', max nbIt nbIt')
      in
      List.fold_left fold (0, 0) mil
    and aux_instr (instr, _pos) =
      match instr with
      | Com.IfThenElse (_, ilThen, ilElse) ->
          let nbRefThen, nbItThen = aux_instrs ilThen in
          let nbRefElse, nbItElse = aux_instrs ilElse in
          (max nbRefThen nbRefElse, max nbItThen nbItElse)
      | Com.WhenDoElse (wdl, ed) ->
          let rec wde (nbRef, nbIt) = function
            | (_, dl, _) :: wdl' ->
                let nbRefD, nbItD = aux_instrs dl in
                wde (max nbRef nbRefD, max nbIt nbItD) wdl'
            | [] ->
                let nbRefD, nbItD = aux_instrs (Pos.unmark ed) in
                (max nbRef nbRefD, max nbIt nbItD)
          in
          wde (0, 0) wdl
      | Com.VerifBlock instrs -> aux_instrs instrs
      | Com.Iterate (_, _, _, instrs) ->
          let nbRef, nbIt = aux_instrs instrs in
          (nbRef + 1, nbIt)
      | Com.Iterate_values (_, _, instrs) ->
          let nbRef, nbIt = aux_instrs instrs in
          (nbRef, nbIt + 1)
      | Com.Restore (_, _, _, _, instrs) ->
          let nbRef, nbIt = aux_instrs instrs in
          (max nbRef 1, nbIt)
      | Com.ArrangeEvents (sort, filter, _, instrs) ->
          let nbItSort = match sort with Some _ -> 2 | None -> 0 in
          let nbItFilter = match filter with Some _ -> 1 | None -> 0 in
          let nbRef, nbIt = aux_instrs instrs in
          (nbRef, max nbIt @@ max nbItSort nbItFilter)
      | Com.Affectation _ | Com.Print _ | Com.ComputeTarget _ | Com.RaiseError _
      | Com.CleanErrors | Com.ExportErrors | Com.FinalizeErrors ->
          (0, 0)
      | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _ ->
          assert false
    in
    let map (t : Validator.target) =
      let nbRef, nbIt = aux_instrs t.target_prog in
      let target_nb_tmps = StrMap.cardinal t.target_tmp_vars + nbIt in
      let target_sz_tmps =
        let fold _ var sz =
          match Com.Var.is_table var with
          | None -> sz + 1
          | Some tab -> sz + Array.length tab
        in
        StrMap.fold fold t.target_tmp_vars nbIt
      in
      let target_nb_refs = List.length t.target_args + nbRef in
      { t with target_nb_tmps; target_sz_tmps; target_nb_refs }
    in
    (StrMap.map map prog.prog_functions, StrMap.map map prog.prog_targets)
  in
  { prog with prog_functions; prog_targets }

let complete_vars (prog : Validator.program) : Validator.program * Mir.stats =
  let prog_vars = prog.prog_vars in
  let prog_vars =
    let incr_cpt cat cpt =
      let i = Com.CatVar.Map.find cat cpt in
      let cpt = Com.CatVar.Map.add cat (i + 1) cpt in
      (cpt, i)
    in
    let cat_cpt = Com.CatVar.Map.map (fun _ -> 0) prog.prog_var_cats in
    let prog_vars, _ =
      StrMap.fold
        (fun vn (var : Com.Var.t) (res, cpt) ->
          let tgv = Com.Var.tgv var in
          let dcat = Com.CatVar.Map.find tgv.cat prog.prog_var_cats in
          let cpt, i = incr_cpt tgv.cat cpt in
          let var = Com.Var.set_loc_tgv_cat var dcat i in
          let res = StrMap.add vn var res in
          (res, cpt))
        prog_vars (StrMap.empty, cat_cpt)
    in
    prog_vars
  in
  let module CatLoc = struct
    type t = Com.CatVar.loc

    let pp fmt (loc : t) =
      match loc with
      | Com.CatVar.LocComputed -> Format.fprintf fmt "calculee"
      | Com.CatVar.LocBase -> Format.fprintf fmt "base"
      | Com.CatVar.LocInput -> Format.fprintf fmt "saisie"

    let compare x y = compare x y
  end in
  let module CatLocMap = struct
    include MapExt.Make (CatLoc)

    let _pp ?(sep = ", ") ?(pp_key = CatLoc.pp) ?(assoc = " => ")
        (pp_val : Format.formatter -> 'a -> unit) (fmt : Format.formatter)
        (map : 'a t) : unit =
      pp ~sep ~pp_key ~assoc pp_val fmt map
  end in
  let loc_vars, sz_loc_vars, sz_vars =
    let fold _ (var : Com.Var.t) (loc_vars, sz_loc_vars, n) =
      let var = Com.Var.set_loc_int var n in
      let loc_cat =
        (Com.CatVar.Map.find (Com.Var.cat var) prog.prog_var_cats).loc
      in
      let loc_vars =
        let upd = function
          | None -> Some (Com.Var.Set.one var)
          | Some set -> Some (Com.Var.Set.add var set)
        in
        CatLocMap.update loc_cat upd loc_vars
      in
      let sz = Com.Var.size var in
      let sz_loc_vars =
        let upd = function
          | None -> Some sz
          | Some n_loc -> Some (n_loc + sz)
        in
        CatLocMap.update loc_cat upd sz_loc_vars
      in
      (loc_vars, sz_loc_vars, n + sz)
    in
    StrMap.fold fold prog_vars (CatLocMap.empty, CatLocMap.empty, 0)
  in
  let update_loc (var : Com.Var.t) (vars, n) =
    let var = Com.Var.set_loc_tgv_idx var n in
    let vars = StrMap.add (Com.Var.name_str var) var vars in
    (vars, n + Com.Var.size var)
  in
  let prog_vars =
    CatLocMap.fold
      (fun _loc_cat vars prog_vars ->
        (prog_vars, 0) |> Com.Var.Set.fold update_loc vars |> fst)
      loc_vars StrMap.empty
  in
  let nb_loc loc_cat =
    match CatLocMap.find_opt loc_cat loc_vars with
    | Some set -> Com.Var.Set.cardinal set
    | None -> 0
  in
  let sz_loc loc_cat =
    match CatLocMap.find_opt loc_cat sz_loc_vars with
    | Some sz -> sz
    | None -> 0
  in
  let stats =
    Mir.
      {
        nb_calculated = nb_loc Com.CatVar.LocComputed;
        nb_input = nb_loc Com.CatVar.LocInput;
        nb_base = nb_loc Com.CatVar.LocBase;
        nb_vars = StrMap.cardinal prog_vars;
        sz_calculated = sz_loc Com.CatVar.LocComputed;
        sz_input = sz_loc Com.CatVar.LocInput;
        sz_base = sz_loc Com.CatVar.LocBase;
        sz_vars;
        nb_all_tmps = 0;
        nb_all_refs = 0;
        sz_all_tmps = 0;
        nb_all_tables = 0;
        sz_all_tables = 0;
        table_map = IntMap.empty;
      }
  in
  ({ prog with prog_vars }, stats)

let complete_stats ((prog : Validator.program), (stats : Mir.stats)) :
    Validator.program * Mir.stats =
  let nb_all_tmps, sz_all_tmps, nb_all_refs =
    let rec aux_instrs tdata mil =
      let fold (nb, sz, nbRef, tdata) mi =
        let nb', sz', nbRef', tdata = aux_instr tdata mi in
        (max nb nb', max sz sz', max nbRef nbRef', tdata)
      in
      List.fold_left fold (0, 0, 0, tdata) mil
    and aux_call tdata name =
      match StrMap.find_opt name tdata with
      | Some (nb, sz, nbRef) -> (nb, sz, nbRef, tdata)
      | None -> (
          let eval_call (t : Validator.target) =
            let nb, sz, nbRef =
              ( t.target_nb_tmps,
                t.target_sz_tmps,
                List.length t.target_args + t.target_nb_refs )
            in
            let nb', sz', nbRef', tdata = aux_instrs tdata t.target_prog in
            let nb = nb + nb' in
            let sz = sz + sz' in
            let nbRef = nbRef + nbRef' in
            let tdata = StrMap.add name (nb, sz, nbRef) tdata in
            (nb, sz, nbRef, tdata)
          in
          match StrMap.find_opt name prog.prog_functions with
          | Some t -> eval_call t
          | None -> eval_call (StrMap.find name prog.prog_targets))
    and aux_instr tdata (instr, _pos) =
      match instr with
      | Com.Affectation mf -> (
          match Pos.unmark mf with
          | SingleFormula (VarDecl (m_access, mei_opt, mev)) -> (
              let nbI, szI, nbRefI, tdata =
                match mei_opt with
                | None -> (0, 0, 0, tdata)
                | Some mei -> aux_expr tdata mei
              in
              let nbV, szV, nbRefV, tdata = aux_expr tdata mev in
              let nb, sz, nbRef =
                (max nbI nbV, max szI szV, max nbRefI nbRefV)
              in
              match Pos.unmark m_access with
              | VarAccess _ -> (nb, sz, nbRef, tdata)
              | TabAccess (_, mi) ->
                  let nbI, szI, nbRefI, tdata = aux_expr tdata mi in
                  (max nbI nb, max szI sz, max nbRefI nbRef, tdata)
              | ConcAccess (_, _, mi) ->
                  let nbI, szI, nbRefI, tdata = aux_expr tdata mi in
                  (max nbI nb, max szI sz, max nbRefI nbRef, tdata)
              | FieldAccess (mei, _, _) ->
                  let nbI, szI, nbRefI, tdata = aux_expr tdata mei in
                  (max nbI nb, max szI sz, max nbRefI nbRef, tdata))
          | SingleFormula (EventFieldRef (mei, _, _, _)) -> aux_expr tdata mei
          | MultipleFormulaes _ -> assert false)
      | Com.ComputeTarget (tn, _args) -> aux_call tdata (Pos.unmark tn)
      | Com.IfThenElse (meI, ilT, ilE) ->
          let nbI, szI, nbRefI, tdata = aux_expr tdata meI in
          let nbT, szT, nbRefT, tdata = aux_instrs tdata ilT in
          let nbE, szE, nbRefE, tdata = aux_instrs tdata ilE in
          let nb = max nbI @@ max nbT nbE in
          let sz = max szI @@ max szT szE in
          let nbRef = max nbRefI @@ max nbRefT nbRefE in
          (nb, sz, nbRef, tdata)
      | Com.WhenDoElse (wdl, ed) ->
          let rec wde (nb, sz, nbRef, tdata) = function
            | (me, dl, _) :: wdl' ->
                let nbE, szE, nbRefE, tdata = aux_expr tdata me in
                let nbD, szD, nbRefD, tdata = aux_instrs tdata dl in
                let nb = max nb @@ max nbE nbD in
                let sz = max sz @@ max szE szD in
                let nbRef = max nbRef @@ max nbRefE nbRefD in
                wde (nb, sz, nbRef, tdata) wdl'
            | [] ->
                let nbD, szD, nbRefD, tdata =
                  aux_instrs tdata (Pos.unmark ed)
                in
                let nb = max nb nbD in
                let sz = max sz szD in
                let nbRef = max nbRef nbRefD in
                (nb, sz, nbRef, tdata)
          in
          wde (0, 0, 0, tdata) wdl
      | Com.VerifBlock instrs -> aux_instrs tdata instrs
      | Com.Print (_, pal) ->
          let fold (nb, sz, nbRef, tdata) (a, _pos) =
            match a with
            | Com.PrintString _ | Com.PrintName _ | Com.PrintAlias _ ->
                (nb, sz, nbRef, tdata)
            | Com.PrintConcName (_, _, me)
            | Com.PrintConcAlias (_, _, me)
            | Com.PrintEventName (me, _, _)
            | Com.PrintEventAlias (me, _, _)
            | Com.PrintIndent me
            | Com.PrintExpr (me, _, _) ->
                let nb', sz', nbRef', tdata = aux_expr tdata me in
                (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          List.fold_left fold (0, 0, 0, tdata) pal
      | Com.Iterate (_, _, mel, instrs) ->
          let fold (nb, sz, nbRef, tdata) (_, me) =
            let nb', sz', nbRef', tdata = aux_expr tdata me in
            (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) mel
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = max nb nb' in
          let sz = max sz sz' in
          let nbRef = 1 + max nbRef nbRef' in
          (nb, sz, nbRef, tdata)
      | Com.Iterate_values (_, me2l, instrs) ->
          let fold (nb, sz, nbRef, tdata) (me0, me1, mstep) =
            let nb', sz', nbRef', tdata = aux_expr tdata me0 in
            let nb'', sz'', nbRef'', tdata = aux_expr tdata me1 in
            let nb''', sz''', nbRef''', tdata = aux_expr tdata mstep in
            let nb = max nb @@ max nb' @@ max nb'' nb''' in
            let sz = max sz @@ max sz' @@ max sz'' sz''' in
            let nbRef = max nbRef @@ max nbRef' @@ max nbRef'' nbRef''' in
            (nb, sz, nbRef, tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) me2l
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = 1 + max nb nb' in
          let sz = 1 + max sz sz' in
          let nbRef = max nbRef nbRef' in
          (nb, sz, nbRef, tdata)
      | Com.Restore (_, var_params, evts, evtfs, instrs) ->
          let nb', sz', nbRef', tdata =
            let fold (nb, sz, nbRef, tdata) (_, _, me) =
              let nb', sz', nbRef', tdata = aux_expr tdata me in
              (max nb nb', max sz sz', max nbRef nbRef', tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) var_params
          in
          let nb'', sz'', nbRef'', tdata =
            let fold (nb, sz, nbRef, tdata) me =
              let nb', sz', nbRef', tdata = aux_expr tdata me in
              (max nb nb', max sz sz', max nbRef nbRef', tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) evts
          in
          let nb''', sz''', nbRef''', tdata =
            let fold (nb, sz, nbRef, tdata) (_, me) =
              let nb', sz', nbRef', tdata = aux_expr tdata me in
              (max nb nb', max sz sz', max nbRef nbRef', tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) evtfs
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = max nb @@ max nb' @@ max nb'' nb''' in
          let sz = max sz @@ max sz' @@ max sz'' sz''' in
          (* ??? *)
          let nbRef = 1 + (max nbRef @@ max nbRef' @@ max nbRef'' nbRef''') in
          (nb, sz, nbRef, tdata)
      | Com.ArrangeEvents (sort, filter, add, instrs) ->
          let n', (nb', sz', nbRef', tdata) =
            match sort with
            | Some (_, _, expr) -> (2, aux_expr tdata expr)
            | None -> (0, (0, 0, 0, tdata))
          in
          let n'', (nb'', sz'', nbRef'', tdata) =
            match filter with
            | Some (_, expr) -> (1, aux_expr tdata expr)
            | None -> (0, (0, 0, 0, tdata))
          in
          let nb''', sz''', nbRef''', tdata =
            match add with
            | Some expr -> aux_expr tdata expr
            | None -> (0, 0, 0, tdata)
          in
          let nb, sz, nbRef, tdata = aux_instrs tdata instrs in
          let nb = max n' n'' + (max nb @@ max nb' @@ max nb'' nb''') in
          let sz = max n' n'' + (max sz @@ max sz' @@ max sz'' sz''') in
          let nbRef = max nbRef @@ max nbRef' @@ max nbRef'' nbRef''' in
          (nb, sz, nbRef, tdata)
      | Com.RaiseError _ | Com.CleanErrors | Com.ExportErrors
      | Com.FinalizeErrors ->
          (0, 0, 0, tdata)
      | Com.ComputeDomain _ | Com.ComputeChaining _ | Com.ComputeVerifs _ ->
          assert false
    and aux_expr tdata (expr, _pos) =
      match expr with
      | Com.TestInSet (_, me, values) ->
          let fold (nb, sz, nbRef, tdata) = function
            | Com.VarValue (TabAccess (_, mei), _)
            | Com.VarValue (ConcAccess (_, _, mei), _)
            | Com.VarValue (FieldAccess (mei, _, _), _) ->
                let nb', sz', nbRef', tdata = aux_expr tdata mei in
                (max nb nb', max sz sz', max nbRef nbRef', tdata)
            | _ -> (nb, sz, nbRef, tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) values
          in
          let nb'', sz'', nbRef'', tdata = aux_expr tdata me in
          (max nb' nb'', max sz' sz'', max nbRef' nbRef'', tdata)
      | Com.Unop (_, me)
      | Com.Index ((VarAccess _, _), me)
      | Com.Var (TabAccess (_, me))
      | Com.Var (ConcAccess (_, _, me))
      | Com.Var (FieldAccess (me, _, _))
      | Com.Size (TabAccess (_, me), _)
      | Com.Size (ConcAccess (_, _, me), _)
      | Com.Size (FieldAccess (me, _, _), _)
      | Com.Attribut ((TabAccess (_, me), _), _)
      | Com.Attribut ((ConcAccess (_, _, me), _), _)
      | Com.Attribut ((FieldAccess (me, _, _), _), _) ->
          aux_expr tdata me
      | Com.Index ((ConcAccess (_, _, me0), _), me1)
      | Com.Index ((FieldAccess (me0, _, _), _), me1)
      | Com.Comparison (_, me0, me1)
      | Com.Binop (_, me0, me1) ->
          let nb0, sz0, nbRef0, tdata = aux_expr tdata me0 in
          let nb1, sz1, nbRef1, tdata = aux_expr tdata me1 in
          (max nb0 nb1, max sz0 sz1, max nbRef0 nbRef1, tdata)
      | Com.Conditional (meI, meT, meEOpt) ->
          let nbI, szI, nbRefI, tdata = aux_expr tdata meI in
          let nbT, szT, nbRefT, tdata = aux_expr tdata meT in
          let nbE, szE, nbRefE, tdata =
            match meEOpt with
            | None -> (0, 0, 0, tdata)
            | Some meE -> aux_expr tdata meE
          in
          let nb = max nbI @@ max nbT nbE in
          let sz = max szI @@ max szT szE in
          let nbRef = max nbRefI @@ max nbRefT nbRefE in
          (nb, sz, nbRef, tdata)
      | Com.FuncCall (func, mel) ->
          let fold (nb, sz, nbRef, tdata) me =
            let nb', sz', nbRef', tdata = aux_expr tdata me in
            (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          let nb', sz', nbRef', tdata =
            List.fold_left fold (0, 0, 0, tdata) mel
          in
          let nb, sz, nbRef, tdata =
            match Pos.unmark func with
            | Func name -> aux_call tdata name
            | _ -> (0, 0, 0, tdata)
          in
          (max nb nb', max sz sz', max nbRef nbRef', tdata)
      | Com.Literal _
      | Com.Var (VarAccess _)
      | Com.NbCategory _ | Com.Attribut _ | Com.Size _ | Com.NbAnomalies
      | Com.NbDiscordances | Com.NbInformatives | Com.NbBloquantes ->
          (0, 0, 0, tdata)
      | Com.Index ((TabAccess _, _), _) | Com.FuncCallLoop _ | Com.Loop _ ->
          assert false
    in
    let nb, sz, nbRef, _ =
      let fold tn _ (nb, sz, nbRef, tdata) =
        let nb', sz', nbRef', tdata = aux_call tdata tn in
        (max nb nb', max sz sz', max nbRef nbRef', tdata)
      in
      (0, 0, 0, StrMap.empty)
      |> StrMap.fold fold prog.prog_functions
      |> StrMap.fold fold prog.prog_targets
    in
    (nb, sz, nbRef)
  in
  (prog, { stats with nb_all_tmps; sz_all_tmps; nb_all_refs })

(** {1 Translation } *)

(** {2 General translation context} *)

let get_var (var_data : Com.Var.t StrMap.t) (name : string Pos.marked) :
    Com.Var.t Pos.marked =
  Pos.same_pos_as (StrMap.find (Pos.unmark name) var_data) name

(** {2 Translation of expressions} *)

let rec translate_expression (p : Validator.program)
    (var_data : Com.Var.t StrMap.t) (f : string Com.m_expression) :
    Mir.m_expression =
  let open Com in
  let expr =
    match Pos.unmark f with
    | TestInSet (positive, e, values) ->
        let new_e = translate_expression p var_data e in
        let new_set_values =
          List.map
            (function
              | FloatValue f -> FloatValue f
              | VarValue (access, pos) ->
                  let access' =
                    match access with
                    | VarAccess v -> VarAccess (StrMap.find v var_data)
                    | TabAccess (m_v, m_i) ->
                        let v, v_pos = m_v in
                        let v' = StrMap.find v var_data in
                        (* faux !!! *)
                        let m_i' = translate_expression p var_data m_i in
                        TabAccess ((v', v_pos), m_i')
                    | ConcAccess (m_vn, m_if, i) ->
                        let i' = translate_expression p var_data i in
                        ConcAccess (m_vn, m_if, i')
                    | FieldAccess (e, ((fn, _) as f), _) ->
                        let e' = translate_expression p var_data e in
                        let i_f = (StrMap.find fn p.prog_event_fields).index in
                        FieldAccess (e', f, i_f)
                  in
                  VarValue (access', pos)
              | IntervalValue (bv, ev) -> IntervalValue (bv, ev))
            (values : string set_value list)
        in
        TestInSet (positive, new_e, new_set_values)
    | Comparison (op, e1, e2) ->
        let new_e1 = translate_expression p var_data e1 in
        let new_e2 = translate_expression p var_data e2 in
        Comparison (op, new_e1, new_e2)
    | Binop (op, e1, e2) ->
        (* if
             Pos.unmark op = Mast.Mul
             && (Pos.unmark e1 = Mast.Literal (Float 0.)
                || Pos.unmark e2 = Mast.Literal (Float 0.))
           then
             (* It is difficult to do a broader or deeper analysis because of
                constant substitutions that could wrongly trigger the warning *)
             Errors.print_spanned_warning
               "Nullifying constant multiplication found." (Pos.get f);*)
        let new_e1 = translate_expression p var_data e1 in
        let new_e2 = translate_expression p var_data e2 in
        Binop (op, new_e1, new_e2)
    | Unop (op, e) ->
        let new_e = translate_expression p var_data e in
        Unop (op, new_e)
    | Index ((access, pos), idx) ->
        let access', pos' =
          match access with
          | VarAccess t ->
              let t_var, t_pos = get_var var_data (t, pos) in
              (VarAccess t_var, t_pos)
          | TabAccess _ -> assert false
          | ConcAccess (m_vn, m_if, i) ->
              let i' = translate_expression p var_data i in
              (ConcAccess (m_vn, m_if, i'), pos)
          | FieldAccess (e, f, _) ->
              let e' = translate_expression p var_data e in
              let i_f =
                (StrMap.find (Pos.unmark f) p.prog_event_fields).index
              in
              (FieldAccess (e', f, i_f), pos)
        in
        let idx' = translate_expression p var_data idx in
        Index ((access', pos'), idx')
    | Conditional (e1, e2, e3) ->
        let new_e1 = translate_expression p var_data e1 in
        let new_e2 = translate_expression p var_data e2 in
        let new_e3 = Option.map (translate_expression p var_data) e3 in
        Conditional (new_e1, new_e2, new_e3)
    | FuncCall (f_name, args) ->
        let new_args =
          List.map (fun arg -> translate_expression p var_data arg) args
        in
        FuncCall (f_name, new_args)
    | Literal l -> Literal l
    | Var access ->
        let access' =
          match access with
          | VarAccess v ->
              let m_v = get_var var_data (Pos.same_pos_as v f) in
              VarAccess (Pos.unmark m_v)
          | TabAccess (m_v, m_i) ->
              let m_v' = get_var var_data m_v in
              let m_i' = translate_expression p var_data m_i in
              TabAccess (m_v', m_i')
          | ConcAccess (m_vn, m_if, i) ->
              let i' = translate_expression p var_data i in
              ConcAccess (m_vn, m_if, i')
          | FieldAccess (e, f, _) ->
              let e' = translate_expression p var_data e in
              let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
              FieldAccess (e', f, i)
        in
        Var access'
    | NbCategory cs -> NbCategory (Validator.mast_to_catvars cs p.prog_var_cats)
    | Attribut ((access, pos), a) -> (
        match access with
        | VarAccess v_name -> (
            let var = StrMap.find v_name var_data in
            if Com.Var.is_ref var then Attribut ((VarAccess var, pos), a)
            else
              match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
              | Some l -> Literal (Float (float (Pos.unmark l)))
              | None -> Literal Undefined)
        | TabAccess (m_v, _) -> (
            let var = StrMap.find (Pos.unmark m_v) var_data in
            match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
            | Some l -> Literal (Float (float (Pos.unmark l)))
            | None -> Literal Undefined)
        | ConcAccess (m_vn, m_if, i) ->
            let i' = translate_expression p var_data i in
            Attribut ((ConcAccess (m_vn, m_if, i'), pos), a)
        | FieldAccess (e, f, _) ->
            let e' = translate_expression p var_data e in
            let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
            Attribut ((FieldAccess (e', f, i), pos), a))
    | Size (access, pos) -> (
        match access with
        | VarAccess v_name ->
            let var = StrMap.find v_name var_data in
            if Com.Var.is_ref var then Size (VarAccess var, pos)
            else Literal (Float (float @@ Com.Var.size var))
        | TabAccess _ -> Literal (Float 1.0)
        | ConcAccess (m_vn, m_if, i) ->
            let i' = translate_expression p var_data i in
            Size (ConcAccess (m_vn, m_if, i'), pos)
        | FieldAccess (e, f, _) ->
            let e' = translate_expression p var_data e in
            let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
            Size (FieldAccess (e', f, i), pos))
    | NbAnomalies -> NbAnomalies
    | NbDiscordances -> NbDiscordances
    | NbInformatives -> NbInformatives
    | NbBloquantes -> NbBloquantes
    | FuncCallLoop _ | Loop _ -> assert false
  in
  Pos.same_pos_as expr f

(** {2 Translation of instructions} *)

let rec translate_prog (p : Validator.program) (var_data : Com.Var.t StrMap.t)
    (it_depth : int) (itval_depth : int) prog =
  let rec aux res = function
    | [] -> List.rev res
    | (Com.Affectation (SingleFormula decl, _), pos) :: il ->
        let decl' =
          match decl with
          | VarDecl (m_access, idx, e) ->
              let m_access' =
                let access, a_pos = m_access in
                match access with
                | VarAccess v ->
                    let v', v_pos' = get_var var_data (v, a_pos) in
                    (Com.VarAccess v', v_pos')
                | TabAccess (m_v, m_i) ->
                    let m_v' = get_var var_data m_v in
                    let m_i' = translate_expression p var_data m_i in
                    (Com.TabAccess (m_v', m_i'), a_pos)
                | ConcAccess (m_vn, m_if, i) ->
                    let i' = translate_expression p var_data i in
                    (Com.ConcAccess (m_vn, m_if, i'), a_pos)
                | FieldAccess (i, f, _) ->
                    let i' = translate_expression p var_data i in
                    let ef = StrMap.find (Pos.unmark f) p.prog_event_fields in
                    (Com.FieldAccess (i', f, ef.index), a_pos)
              in
              let idx' = Option.map (translate_expression p var_data) idx in
              let e' = translate_expression p var_data e in
              Com.VarDecl (m_access', idx', e')
          | EventFieldRef (idx, f, _, v) ->
              let idx' = translate_expression p var_data idx in
              let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
              let v' = get_var var_data v in
              Com.EventFieldRef (idx', f, i, v')
        in
        let m_form = (Com.SingleFormula decl', pos) in
        aux ((Com.Affectation m_form, pos) :: res) il
    | (Com.Affectation (MultipleFormulaes _, _), _) :: _ -> assert false
    | (Com.IfThenElse (e, ilt, ile), pos) :: il ->
        let expr = translate_expression p var_data e in
        let prog_then = aux [] ilt in
        let prog_else = aux [] ile in
        aux ((Com.IfThenElse (expr, prog_then, prog_else), pos) :: res) il
    | (Com.WhenDoElse (wdl, ed), pos) :: il ->
        let map_wdl (expr, dl, pos) =
          let expr' = translate_expression p var_data expr in
          let dl' = aux [] dl in
          (expr', dl', pos)
        in
        let wdl' = List.map map_wdl wdl in
        let ed' = Pos.same_pos_as (aux [] (Pos.unmark ed)) ed in
        aux ((Com.WhenDoElse (wdl', ed'), pos) :: res) il
    | (Com.ComputeTarget (tn, targs), pos) :: il ->
        let map v = get_var var_data v in
        let targs' = List.map map targs in
        aux ((Com.ComputeTarget (tn, targs'), pos) :: res) il
    | (Com.VerifBlock instrs, pos) :: il ->
        let instrs' = aux [] instrs in
        aux ((Com.VerifBlock instrs', pos) :: res) il
    | (Com.Print (std, args), pos) :: il ->
        let mir_args =
          List.rev
            (List.fold_left
               (fun res arg ->
                 let mir_arg =
                   match Pos.unmark arg with
                   | Com.PrintString s -> Com.PrintString s
                   | Com.PrintName v -> (
                       match StrMap.find_opt (Pos.unmark v) var_data with
                       | Some var ->
                           if Com.Var.is_ref var then
                             Com.PrintName (Pos.same_pos_as var v)
                           else Com.PrintString (Pos.unmark var.name)
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" (Pos.unmark v)
                           in
                           Errors.raise_spanned_error msg (Pos.get v))
                   | Com.PrintAlias v -> (
                       match StrMap.find_opt (Pos.unmark v) var_data with
                       | Some var ->
                           if Com.Var.is_ref var then
                             Com.PrintAlias (Pos.same_pos_as var v)
                           else Com.PrintString (Com.Var.alias_str var)
                       | _ ->
                           let msg =
                             Format.sprintf "unknown variable %s" (Pos.unmark v)
                           in
                           Errors.raise_spanned_error msg (Pos.get v))
                   | Com.PrintConcName (m_vn, m_if, i) ->
                       let i' = translate_expression p var_data i in
                       Com.PrintConcName (m_vn, m_if, i')
                   | Com.PrintConcAlias (m_vn, m_if, i) ->
                       let i' = translate_expression p var_data i in
                       Com.PrintConcAlias (m_vn, m_if, i')
                   | Com.PrintEventName (e, f, _) ->
                       let e' = translate_expression p var_data e in
                       let i =
                         (StrMap.find (Pos.unmark f) p.prog_event_fields).index
                       in
                       Com.PrintEventName (e', f, i)
                   | Com.PrintEventAlias (e, f, _) ->
                       let e' = translate_expression p var_data e in
                       let i =
                         (StrMap.find (Pos.unmark f) p.prog_event_fields).index
                       in
                       Com.PrintEventAlias (e', f, i)
                   | Com.PrintIndent e ->
                       Com.PrintIndent (translate_expression p var_data e)
                   | Com.PrintExpr (e, min, max) ->
                       Com.PrintExpr
                         (translate_expression p var_data e, min, max)
                 in
                 Pos.same_pos_as mir_arg arg :: res)
               [] args)
        in
        aux ((Com.Print (std, mir_args), pos) :: res) il
    | (Com.Iterate (vn, vars, var_params, instrs), pos) :: il ->
        let var_pos = Pos.get vn in
        let var_name = Pos.unmark vn in
        (match StrMap.find_opt var_name var_data with
        | Some v ->
            let msg =
              Format.asprintf "variable already declared %a" Pos.format_position
                (Pos.get v.name)
            in
            Errors.raise_spanned_error msg pos
        | _ -> ());
        let var = Com.Var.new_ref ~name:(var_name, var_pos) ~loc_int:it_depth in
        let var_data = StrMap.add var_name var var_data in
        let vars' =
          List.map
            (fun vn ->
              Pos.same_pos_as (StrMap.find (Pos.unmark vn) var_data) vn)
            vars
        in
        let var_params' =
          List.map
            (fun (vcats, expr) ->
              let catSet = Validator.mast_to_catvars vcats p.prog_var_cats in
              let mir_expr = translate_expression p var_data expr in
              (catSet, mir_expr))
            var_params
        in
        let prog_it =
          translate_prog p var_data (it_depth + 1) itval_depth instrs
        in
        let m_var = Pos.same_pos_as var vn in
        aux ((Com.Iterate (m_var, vars', var_params', prog_it), pos) :: res) il
    | (Com.Iterate_values (vn, var_intervals, instrs), pos) :: il ->
        let var_pos = Pos.get vn in
        let var_name = Pos.unmark vn in
        (match StrMap.find_opt var_name var_data with
        | Some v ->
            let msg =
              Format.asprintf "variable already declared %a" Pos.format_position
                (Pos.get v.name)
            in
            Errors.raise_spanned_error msg pos
        | _ -> ());
        let var =
          Com.Var.new_temp ~name:(var_name, var_pos) ~is_table:None
            ~loc_int:itval_depth
        in
        let var_data = StrMap.add var_name var var_data in
        let var_intervals' =
          List.map
            (fun (e0, e1, step) ->
              let e0' = translate_expression p var_data e0 in
              let e1' = translate_expression p var_data e1 in
              let step' = translate_expression p var_data step in
              (e0', e1', step'))
            var_intervals
        in
        let prog_it =
          translate_prog p var_data it_depth (itval_depth + 1) instrs
        in
        let m_var = Pos.same_pos_as var vn in
        aux
          ((Com.Iterate_values (m_var, var_intervals', prog_it), pos) :: res)
          il
    | (Com.Restore (vars, var_params, evts, evtfs, instrs), pos) :: il ->
        let vars' =
          List.map
            (fun vn ->
              Pos.same_pos_as (StrMap.find (Pos.unmark vn) var_data) vn)
            vars
        in
        let var_params' =
          List.map
            (fun (vn, vcats, expr) ->
              let var_pos = Pos.get vn in
              let var_name = Pos.unmark vn in
              let var =
                Com.Var.new_ref ~name:(var_name, var_pos) ~loc_int:it_depth
              in
              let var_data = StrMap.add var_name var var_data in
              let catSet = Validator.mast_to_catvars vcats p.prog_var_cats in
              let mir_expr = translate_expression p var_data expr in
              (Pos.mark var var_pos, catSet, mir_expr))
            var_params
        in
        let evts' = List.map (translate_expression p var_data) evts in
        let evtfs' =
          List.map
            (fun (vn, expr) ->
              let var_pos = Pos.get vn in
              let var_name = Pos.unmark vn in
              let var =
                Com.Var.new_temp ~name:(var_name, var_pos) ~is_table:None
                  ~loc_int:itval_depth
              in
              let var_data = StrMap.add var_name var var_data in
              let mir_expr = translate_expression p var_data expr in
              (Pos.mark var var_pos, mir_expr))
            evtfs
        in
        let prog_rest = translate_prog p var_data it_depth itval_depth instrs in
        aux
          ((Com.Restore (vars', var_params', evts', evtfs', prog_rest), pos)
          :: res)
          il
    | (Com.ArrangeEvents (sort, filter, add, instrs), pos) :: il ->
        let sort', itval_depth' =
          match sort with
          | Some (var0, var1, expr) ->
              let var0_pos = Pos.get var0 in
              let var0_name = Pos.unmark var0 in
              (match StrMap.find_opt var0_name var_data with
              | Some v ->
                  let msg =
                    Format.asprintf "variable already declared %a"
                      Pos.format_position (Pos.get v.name)
                  in
                  Errors.raise_spanned_error msg pos
              | _ -> ());
              let var0' =
                Com.Var.new_temp ~name:(var0_name, var0_pos) ~is_table:None
                  ~loc_int:itval_depth
              in
              let var1_pos = Pos.get var1 in
              let var1_name = Pos.unmark var1 in
              (match StrMap.find_opt var1_name var_data with
              | Some v ->
                  let msg =
                    Format.asprintf "variable already declared %a"
                      Pos.format_position (Pos.get v.name)
                  in
                  Errors.raise_spanned_error msg pos
              | _ -> ());
              let var1' =
                Com.Var.new_temp ~name:(var1_name, var1_pos) ~is_table:None
                  ~loc_int:(itval_depth + 1)
              in
              let var_data =
                var_data |> StrMap.add var0_name var0'
                |> StrMap.add var1_name var1'
              in
              let m_var0 = Pos.same_pos_as var0' var0 in
              let m_var1 = Pos.same_pos_as var1' var1 in
              let expr' = translate_expression p var_data expr in
              (Some (m_var0, m_var1, expr'), itval_depth + 2)
          | None -> (None, itval_depth)
        in
        let filter', itval_depth' =
          match filter with
          | Some (var, expr) ->
              let var_pos = Pos.get var in
              let var_name = Pos.unmark var in
              (match StrMap.find_opt var_name var_data with
              | Some v ->
                  let msg =
                    Format.asprintf "variable already declared %a"
                      Pos.format_position (Pos.get v.name)
                  in
                  Errors.raise_spanned_error msg pos
              | _ -> ());
              let var' =
                Com.Var.new_temp ~name:(var_name, var_pos) ~is_table:None
                  ~loc_int:itval_depth
              in
              let var_data = StrMap.add var_name var' var_data in
              let m_var = Pos.same_pos_as var' var in
              let expr' = translate_expression p var_data expr in
              (Some (m_var, expr'), max itval_depth' (itval_depth + 1))
          | None -> (None, itval_depth')
        in
        let add' = Option.map (translate_expression p var_data) add in
        let instrs' = translate_prog p var_data it_depth itval_depth' instrs in
        aux ((Com.ArrangeEvents (sort', filter', add', instrs'), pos) :: res) il
    | (Com.RaiseError (err_name, var_opt), pos) :: il ->
        let err_decl = StrMap.find (Pos.unmark err_name) p.prog_errors in
        let m_err_decl = Pos.same_pos_as err_decl err_name in
        aux ((Com.RaiseError (m_err_decl, var_opt), pos) :: res) il
    | (Com.CleanErrors, pos) :: il -> aux ((Com.CleanErrors, pos) :: res) il
    | (Com.ExportErrors, pos) :: il -> aux ((Com.ExportErrors, pos) :: res) il
    | (Com.FinalizeErrors, pos) :: il ->
        aux ((Com.FinalizeErrors, pos) :: res) il
    | (Com.ComputeDomain _, _) :: _
    | (Com.ComputeChaining _, _) :: _
    | (Com.ComputeVerifs (_, _), _) :: _ ->
        assert false
  in
  aux [] prog

let get_targets (is_function : bool) (p : Validator.program)
    (var_data : Com.Var.t StrMap.t) (ts : Validator.target StrMap.t) :
    Mir.target StrMap.t =
  StrMap.fold
    (fun _ (t : Validator.target) targets ->
      let target_name = t.target_name in
      let target_file = t.target_file in
      let target_apps = t.target_apps in
      let target_nb_refs = t.target_nb_refs in
      let tmp_var_data, _ =
        let idx_init = if is_function then 0 else -target_nb_refs in
        List.fold_left
          (fun (tmp_var_data, n) var ->
            let var = Com.Var.set_loc_int var n in
            let name = Com.Var.name_str var in
            let tmp_var_data = StrMap.add name var tmp_var_data in
            (tmp_var_data, n + 1))
          (var_data, idx_init) t.target_args
      in
      let target_args =
        let map var = StrMap.find (Com.Var.name_str var) tmp_var_data in
        List.map map t.target_args
      in
      let target_sz_tmps = t.target_sz_tmps in
      let tmp_var_data, itval_depth =
        StrMap.fold
          (fun name var (tmp_var_data, n) ->
            let var = Com.Var.set_loc_int var n in
            let tmp_var_data = StrMap.add name var tmp_var_data in
            (tmp_var_data, n + Com.Var.size var))
          t.target_tmp_vars
          (tmp_var_data, -target_sz_tmps)
      in
      let tmp_var_data =
        StrMap.fold
          (fun name var tmp_var_data ->
            let var = StrMap.find (Com.Var.name_str var) tmp_var_data in
            match Com.Var.is_table var with
            | Some tab ->
                let is_table =
                  let map var =
                    StrMap.find (Com.Var.name_str var) tmp_var_data
                  in
                  Some (Array.map map tab)
                in
                let var = Com.Var.set_is_table var is_table in
                StrMap.add name var tmp_var_data
            | None -> tmp_var_data)
          t.target_tmp_vars tmp_var_data
      in
      let tmp_var_data =
        if is_function then
          let var = Option.get t.target_result in
          StrMap.add (Com.Var.name_str var) var tmp_var_data
        else tmp_var_data
      in
      let target_result = t.target_result in
      let target_tmp_vars =
        let map var = StrMap.find (Com.Var.name_str var) tmp_var_data in
        StrMap.map map t.target_tmp_vars
      in
      let target_prog =
        translate_prog p tmp_var_data
          (List.length target_args - target_nb_refs)
          itval_depth t.target_prog
      in
      let target =
        Com.
          {
            target_name;
            target_file;
            target_apps;
            target_args;
            target_result;
            target_tmp_vars;
            target_prog;
            target_nb_tmps = t.target_nb_tmps;
            target_sz_tmps;
            target_nb_refs;
          }
      in
      StrMap.add (Pos.unmark target_name) target targets)
    ts StrMap.empty

let complete_tables (p : Mir.program) : Mir.program =
  let p, map, nb_all, sz_all =
    let fold_vars vname v (program_vars, map, nb_all, sz_all) =
      match Com.Var.is_table v with
      | None -> (program_vars, map, nb_all, sz_all)
      | Some tab ->
          let nb_all = nb_all + 1 in
          let vsz = Com.Var.size v in
          let map, tab =
            let rec loop map tab i =
              if i = vsz then (map, tab)
              else
                let iVar =
                  StrMap.find (Com.Var.name_str tab.(i)) program_vars
                in
                let map = IntMap.add (sz_all + i) iVar map in
                tab.(i) <- iVar;
                loop map tab (i + 1)
            in
            loop map tab 0
          in
          let v = Com.Var.set_loc_int v sz_all in
          let v = Com.Var.set_is_table v (Some tab) in
          let _program_vars = StrMap.add vname v program_vars in
          (* !!! *)
          let sz_all = sz_all + vsz in
          (program_vars, map, nb_all, sz_all)
    in
    let program_vars, map, nb_all, sz_all =
      StrMap.fold fold_vars p.program_vars (p.program_vars, IntMap.empty, 0, 0)
    in
    let p = { p with program_vars } in
    (p, map, nb_all, sz_all)
  in
  let stat_targets prog_tarfuns map nb_all sz_all =
    let fold_targets tname target (prog_tarfuns, map, nb_all, sz_all) =
      let fold_tmps vn var (target_tmp_vars, map, nb_all, sz_all) =
        match Com.Var.is_table var with
        | None -> (target_tmp_vars, map, nb_all, sz_all)
        | Some tab ->
            let nb_all = nb_all + 1 in
            let var = Com.Var.set_loc_int var sz_all in
            let vsz = Com.Var.size var in
            let tab =
              let init i =
                StrMap.find (Com.Var.name_str tab.(i)) target_tmp_vars
              in
              Array.init vsz init
            in
            let var = Com.Var.set_is_table var (Some tab) in
            let _target_tmp_vars = StrMap.add vn var target_tmp_vars in
            (* !!! *)
            let map =
              let rec loop map i =
                if i = vsz then map
                else
                  let map = IntMap.add (sz_all + i) tab.(i) map in
                  loop map (i + 1)
              in
              loop map 0
            in
            let sz_all = sz_all + vsz in
            (target_tmp_vars, map, nb_all, sz_all)
      in
      let target_tmp_vars, map, nb_all, sz_all =
        StrMap.fold fold_tmps target.Com.target_tmp_vars
          (target.target_tmp_vars, map, nb_all, sz_all)
      in
      let target = { target with target_tmp_vars } in
      let prog_tarfuns = StrMap.add tname target prog_tarfuns in
      (prog_tarfuns, map, nb_all, sz_all)
    in
    StrMap.fold fold_targets prog_tarfuns (prog_tarfuns, map, nb_all, sz_all)
  in
  let p, map, nb_all, sz_all =
    let program_functions, map, nb_all, sz_all =
      stat_targets p.program_functions map nb_all sz_all
    in
    let p = { p with program_functions } in
    (p, map, nb_all, sz_all)
  in
  let p, map, nb_all, sz_all =
    let program_targets, map, nb_all, sz_all =
      stat_targets p.program_targets map nb_all sz_all
    in
    let p = { p with program_targets } in
    (p, map, nb_all, sz_all)
  in
  let program_stats =
    {
      p.program_stats with
      nb_all_tables = nb_all;
      sz_all_tables = sz_all;
      table_map = map;
    }
  in
  { p with program_stats }

let translate (p : Validator.program) : Mir.program =
  let p, program_stats =
    p |> complete_vars_stack |> complete_vars |> complete_stats
  in
  let program_rules =
    let map_rule (rule : Validator.rule) =
      let id = Pos.unmark rule.rule_id in
      Format.sprintf "%s_regle_%d" p.prog_prefix id
    in
    IntMap.map map_rule p.prog_rules
  in
  let program_verifs =
    let map_verif (verif : Validator.verif) =
      let id = Pos.unmark verif.verif_id in
      Format.sprintf "%s_verif_%d" p.prog_prefix id
    in
    IntMap.map map_verif p.prog_verifs
  in
  let program_chainings =
    let map_chainings (chaining : Validator.chaining) =
      let name = Pos.unmark chaining.chain_name in
      Format.sprintf "%s_chaining_%s" p.prog_prefix name
    in
    StrMap.map map_chainings p.prog_chainings
  in
  let program_functions = get_targets true p p.prog_vars p.prog_functions in
  let program_targets = get_targets false p p.prog_vars p.prog_targets in
  let mir_program =
    Mir.
      {
        program_safe_prefix = p.prog_prefix;
        program_applications = p.prog_apps;
        program_var_categories = p.prog_var_cats;
        program_rule_domains = p.prog_rdoms;
        program_verif_domains = p.prog_vdoms;
        program_vars = p.prog_vars;
        program_alias = p.prog_alias;
        program_event_fields = p.prog_event_fields;
        program_event_field_idxs = p.prog_event_field_idxs;
        program_rules;
        program_verifs;
        program_chainings;
        program_errors = p.prog_errors;
        program_functions;
        program_targets;
        program_main_target = p.prog_main_target;
        program_stats;
      }
  in
  mir_program |> complete_tables
