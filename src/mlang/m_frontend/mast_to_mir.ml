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
        let fold _ m_id sz =
          let var = IntMap.find (Pos.unmark m_id) prog.prog_dict in
          match Com.Var.get_table var with
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
  let tgv_dict =
    let fold _ v set =
      if Com.Var.is_tgv v then Com.Var.Set.add v set else set
    in
    IntMap.fold fold prog.prog_dict Com.Var.Set.empty
  in
  let tgv_dict =
    let incr_cpt cat cpt =
      let i = Com.CatVar.Map.find cat cpt in
      let cpt = Com.CatVar.Map.add cat (i + 1) cpt in
      (cpt, i)
    in
    let cat_cpt = Com.CatVar.Map.map (fun _ -> 0) prog.prog_var_cats in
    fst
    @@ Com.Var.Set.fold
         (fun (var : Com.Var.t) (res, cpt) ->
           let tgv = Com.Var.tgv var in
           let dcat = Com.CatVar.Map.find tgv.cat prog.prog_var_cats in
           let cpt, i = incr_cpt tgv.cat cpt in
           let var = Com.Var.set_loc_tgv_idx var dcat i in
           let res = Com.Var.Set.add var res in
           (res, cpt))
         tgv_dict
         (Com.Var.Set.empty, cat_cpt)
  in
  let module CatLoc = struct
    type t = Com.CatVar.loc

    let compare x y = compare x y
  end in
  let module CatLocMap = MapExt.Make (CatLoc) in
  let loc_vars, sz_loc_vars, sz_vars =
    let fold (var : Com.Var.t) (loc_vars, sz_loc_vars, n) =
      if Com.Var.is_tgv var then
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
      else (loc_vars, sz_loc_vars, n)
    in
    Com.Var.Set.fold fold tgv_dict (CatLocMap.empty, CatLocMap.empty, 0)
  in
  let update_loc (var : Com.Var.t) (vars, n) =
    let var = Com.Var.set_loc_idx var n in
    let vars = Com.Var.Set.add var vars in
    (vars, n + Com.Var.size var)
  in
  let tgv_dict =
    CatLocMap.fold
      (fun _loc_cat vars dict ->
        (dict, 0) |> Com.Var.Set.fold update_loc vars |> fst)
      loc_vars Com.Var.Set.empty
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
  let prog_dict =
    let fold (v : Com.Var.t) res = IntMap.add v.id v res in
    Com.Var.Set.fold fold tgv_dict prog.prog_dict
  in
  let prog_dict =
    let fold id v (prog_dict, n) =
      if Com.Var.is_temp v then
        let v = Com.Var.set_loc_tmp_idx v n in
        let prog_dict = IntMap.add id v prog_dict in
        (prog_dict, n + 1)
      else (prog_dict, n)
    in
    fst @@ IntMap.fold fold prog_dict (prog_dict, 0)
  in
  let prog = { prog with prog_dict } in
  let stats =
    Mir.
      {
        nb_calculated = nb_loc Com.CatVar.LocComputed;
        nb_input = nb_loc Com.CatVar.LocInput;
        nb_base = nb_loc Com.CatVar.LocBase;
        nb_vars = StrMap.cardinal prog.prog_vars;
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
  (prog, stats)

let complete_target_vars ((prog : Validator.program), (stats : Mir.stats)) :
    Validator.program * Mir.stats =
  let fold is_function _ (t : Validator.target) prog_dict =
    let prog_dict, _ =
      let idx_init = if is_function then 0 else -t.target_nb_refs in
      List.fold_left
        (fun (prog_dict, n) m_id ->
          let var = IntMap.find (Pos.unmark m_id) prog_dict in
          let var = Com.Var.set_loc_idx var n in
          let prog_dict = IntMap.add var.id var prog_dict in
          (prog_dict, n + 1))
        (prog_dict, idx_init) t.target_args
    in
    let prog_dict, _ =
      StrMap.fold
        (fun _name m_id (prog_dict, n) ->
          let var = IntMap.find (Pos.unmark m_id) prog_dict in
          let var = Com.Var.set_loc_idx var n in
          let prog_dict = IntMap.add var.id var prog_dict in
          (prog_dict, n + Com.Var.size var))
        t.target_tmp_vars
        (prog_dict, -t.target_sz_tmps)
    in
    StrMap.fold
      (fun _name m_id prog_dict ->
        let var = IntMap.find (Pos.unmark m_id) prog_dict in
        match Com.Var.get_table var with
        | Some tab ->
            let table =
              let map (v : Com.Var.t) = IntMap.find v.id prog_dict in
              Some (Array.map map tab)
            in
            let var = Com.Var.set_table var table in
            IntMap.add var.id var prog_dict
        | None -> prog_dict)
      t.target_tmp_vars prog_dict
  in
  let prog_dict =
    prog.prog_dict
    |> StrMap.fold (fold false) prog.prog_targets
    |> StrMap.fold (fold true) prog.prog_functions
  in
  let prog = { prog with prog_dict } in
  (prog, stats)

let complete_tabs ((prog : Validator.program), (stats : Mir.stats)) :
    Validator.program * Mir.stats =
  let prog_dict, table_map, nb_all_tables, sz_all_tables =
    let map_add var map = IntMap.add (IntMap.cardinal map) var map in
    let fold_vars id v (prog_dict, map, nb_all, sz_all) =
      match Com.Var.get_table v with
      | None -> (prog_dict, map, nb_all, sz_all)
      | Some tab ->
          let nb_all = nb_all + 1 in
          let vsz = Com.Var.size v in
          let v = Com.Var.set_loc_idx v (IntMap.cardinal map) in
          let map = map_add v map in
          let map, tab =
            let rec loop map tab i =
              if i = vsz then (map, tab)
              else
                let iVar = IntMap.find tab.(i).Com.Var.id prog_dict in
                let map = map_add iVar map in
                tab.(i) <- iVar;
                loop map tab (i + 1)
            in
            loop map tab 0
          in
          let v = Com.Var.set_table v (Some tab) in
          let prog_dict = IntMap.add id v prog_dict in
          let sz_all = sz_all + vsz in
          (prog_dict, map, nb_all, sz_all)
    in
    IntMap.fold fold_vars prog.prog_dict (prog.prog_dict, IntMap.empty, 0, 0)
  in
  let prog = { prog with prog_dict } in
  let stats = { stats with nb_all_tables; sz_all_tables; table_map } in
  (prog, stats)

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
          | SingleFormula (VarDecl (m_access, mev)) -> (
              let nb, sz, nbRef, tdata = aux_expr tdata mev in
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
      | Com.Comparison (_, me0, me1) | Com.Binop (_, me0, me1) ->
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
      | Com.FuncCallLoop _ | Com.Loop _ -> assert false
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

let get_var (var_data : Com.Var.t IntMap.t) (m_id : int Pos.marked) : Com.Var.t
    =
  IntMap.find (Pos.unmark m_id) var_data

(** {2 Translation of expressions} *)

let rec translate_expression (p : Validator.program)
    (var_data : Com.Var.t IntMap.t) (f : int Pos.marked Com.m_expression) :
    Mir.m_expression =
  let open Com in
  let expr =
    match Pos.unmark f with
    | TestInSet (positive, m_e, values) ->
        let new_e = translate_expression p var_data m_e in
        let new_set_values =
          List.map
            (function
              | FloatValue f -> FloatValue f
              | VarValue (access, pos) ->
                  let access' =
                    match access with
                    | VarAccess m_id ->
                        let v' = get_var var_data m_id in
                        VarAccess v'
                    | TabAccess (m_id, m_i) ->
                        let v' = get_var var_data m_id in
                        let m_i' = translate_expression p var_data m_i in
                        TabAccess (v', m_i')
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
            (values : int Pos.marked set_value list)
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
          | VarAccess m_v -> VarAccess (get_var var_data m_v)
          | TabAccess (m_v, m_i) ->
              let v' = get_var var_data m_v in
              let m_i' = translate_expression p var_data m_i in
              TabAccess (v', m_i')
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
        | VarAccess m_id -> (
            let var = get_var var_data m_id in
            if Com.Var.is_ref var then
              Attribut (Pos.same_pos_as (VarAccess var) m_id, a)
            else
              match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
              | Some l -> Literal (Float (float (Pos.unmark l)))
              | None -> Literal Undefined)
        | TabAccess (m_id, _) -> (
            let var = get_var var_data m_id in
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
        | VarAccess m_id ->
            let var = get_var var_data m_id in
            if Com.Var.is_ref var then
              Size (Pos.same_pos_as (VarAccess var) m_id)
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

let rec translate_prog (p : Validator.program) (var_data : Com.Var.t IntMap.t)
    (it_depth : int) (itval_depth : int) prog =
  let rec aux res = function
    | [] -> List.rev res
    | (Com.Affectation (SingleFormula decl, _), pos) :: il ->
        let decl' =
          match decl with
          | VarDecl (m_access, e) ->
              let m_access' =
                let access, a_pos = m_access in
                match access with
                | VarAccess m_v ->
                    let v' = get_var var_data m_v in
                    (Com.VarAccess v', a_pos)
                | TabAccess (m_v, m_i) ->
                    let v' = get_var var_data m_v in
                    let m_i' = translate_expression p var_data m_i in
                    (Com.TabAccess (v', m_i'), a_pos)
                | ConcAccess (m_vn, m_if, i) ->
                    let i' = translate_expression p var_data i in
                    (Com.ConcAccess (m_vn, m_if, i'), a_pos)
                | FieldAccess (i, f, _) ->
                    let i' = translate_expression p var_data i in
                    let ef = StrMap.find (Pos.unmark f) p.prog_event_fields in
                    (Com.FieldAccess (i', f, ef.index), a_pos)
              in
              let e' = translate_expression p var_data e in
              Com.VarDecl (m_access', e')
          | EventFieldRef (idx, f, _, m_v) ->
              let idx' = translate_expression p var_data idx in
              let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
              let v' = get_var var_data m_v in
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
                   | Com.PrintName m_id ->
                       let var = get_var var_data m_id in
                       if Com.Var.is_ref var then Com.PrintName var
                       else Com.PrintString (Pos.unmark var.name)
                   | Com.PrintAlias m_id ->
                       let var = get_var var_data m_id in
                       if Com.Var.is_ref var then Com.PrintAlias var
                       else Com.PrintString (Com.Var.alias_str var)
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
    | (Com.Iterate (m_id, vars, var_params, instrs), pos) :: il ->
        let var = get_var var_data m_id in
        let var = Com.Var.set_loc_idx var it_depth in
        let var_data = IntMap.add var.id var var_data in
        let vars' = List.map (get_var var_data) vars in
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
        aux ((Com.Iterate (var, vars', var_params', prog_it), pos) :: res) il
    | (Com.Iterate_values (m_id, var_intervals, instrs), pos) :: il ->
        let var = get_var var_data m_id in
        let var = Com.Var.set_loc_idx var itval_depth in
        let var_data = IntMap.add var.id var var_data in
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
        aux ((Com.Iterate_values (var, var_intervals', prog_it), pos) :: res) il
    | (Com.Restore (vars, var_params, evts, evtfs, instrs), pos) :: il ->
        let vars' = List.map (get_var var_data) vars in
        let var_params' =
          List.map
            (fun (m_id, vcats, expr) ->
              let var = get_var var_data m_id in
              let var = Com.Var.set_loc_idx var it_depth in
              let var_data = IntMap.add var.id var var_data in
              let catSet = Validator.mast_to_catvars vcats p.prog_var_cats in
              let mir_expr = translate_expression p var_data expr in
              (var, catSet, mir_expr))
            var_params
        in
        let evts' = List.map (translate_expression p var_data) evts in
        let evtfs' =
          List.map
            (fun (m_id, expr) ->
              let var = get_var var_data m_id in
              let var = Com.Var.set_loc_idx var itval_depth in
              let var_data = IntMap.add var.id var var_data in
              let mir_expr = translate_expression p var_data expr in
              (var, mir_expr))
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
          | Some (m_id0, m_id1, expr) ->
              let var0 = get_var var_data m_id0 in
              let var0' = Com.Var.set_loc_idx var0 itval_depth in
              let var1 = get_var var_data m_id1 in
              let var1' = Com.Var.set_loc_idx var1 (itval_depth + 1) in
              let var_data =
                var_data |> IntMap.add var0.id var0' |> IntMap.add var1.id var1'
              in
              let expr' = translate_expression p var_data expr in
              (Some (var0', var1', expr'), itval_depth + 2)
          | None -> (None, itval_depth)
        in
        let filter', itval_depth' =
          match filter with
          | Some (m_id, expr) ->
              let var = get_var var_data m_id in
              let var' = Com.Var.set_loc_idx var itval_depth in
              let var_data = IntMap.add var.id var' var_data in
              let expr' = translate_expression p var_data expr in
              (Some (var', expr'), max itval_depth' (itval_depth + 1))
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

let get_targets (p : Validator.program) (ts : Validator.target StrMap.t) :
    Mir.target StrMap.t =
  StrMap.fold
    (fun _ (t : Validator.target) targets ->
      let target_name = t.target_name in
      let target_file = t.target_file in
      let target_apps = t.target_apps in
      let target_nb_tmps = t.target_nb_tmps in
      let target_nb_refs = t.target_nb_refs in
      let target_args =
        let map m_id = IntMap.find (Pos.unmark m_id) p.prog_dict in
        List.map map t.target_args
      in
      let target_sz_tmps = t.target_sz_tmps in
      let itval_depth =
        StrMap.fold
          (fun _name m_id n ->
            let var = IntMap.find (Pos.unmark m_id) p.prog_dict in
            n + Com.Var.size var)
          t.target_tmp_vars (-target_sz_tmps)
      in
      let target_result =
        let map m_id = IntMap.find (Pos.unmark m_id) p.prog_dict in
        Option.map map t.target_result
      in
      let target_tmp_vars =
        let map m_id = IntMap.find (Pos.unmark m_id) p.prog_dict in
        StrMap.map map t.target_tmp_vars
      in
      let target_prog =
        translate_prog p p.prog_dict
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
            target_nb_tmps;
            target_sz_tmps;
            target_nb_refs;
          }
      in
      StrMap.add (Pos.unmark target_name) target targets)
    ts StrMap.empty

let translate (p : Validator.program) : Mir.program =
  let p, program_stats =
    p |> complete_vars_stack |> complete_vars |> complete_target_vars
    |> complete_stats |> complete_tabs
  in
  let program_vars, program_alias =
    let map id = IntMap.find id p.prog_dict in
    (StrMap.map map p.prog_vars, StrMap.map map p.prog_alias)
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
  let program_functions = get_targets p p.prog_functions in
  let program_targets = get_targets p p.prog_targets in
  Mir.
    {
      program_safe_prefix = p.prog_prefix;
      program_applications = p.prog_apps;
      program_var_categories = p.prog_var_cats;
      program_rule_domains = p.prog_rdoms;
      program_verif_domains = p.prog_vdoms;
      program_vars;
      program_alias;
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
