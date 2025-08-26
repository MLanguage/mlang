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
    and aux_instr (Pos.Mark (instr, _pos)) =
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
      let is_f = Com.target_is_function t in
      let nb_args = List.length t.target_args in
      let target_nb_tmps =
        StrMap.cardinal t.target_tmp_vars
        + nbIt
        + if is_f then 1 + nb_args else 0
      in
      let target_sz_tmps =
        let fold _ m_id sz =
          let var = IntMap.find (Pos.unmark m_id) prog.prog_dict in
          match Com.Var.get_table var with
          | None -> sz + 1
          | Some tab -> sz + Array.length tab
        in
        StrMap.fold fold t.target_tmp_vars
          (nbIt + if is_f then 1 + nb_args else 0)
      in
      let target_nb_refs = nbRef + if is_f then 0 else nb_args in
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
        nb_computed = nb_loc Com.CatVar.LocComputed;
        nb_input = nb_loc Com.CatVar.LocInput;
        nb_base = nb_loc Com.CatVar.LocBase;
        nb_vars = StrMap.cardinal prog.prog_vars;
        sz_computed = sz_loc Com.CatVar.LocComputed;
        sz_input = sz_loc Com.CatVar.LocInput;
        sz_base = sz_loc Com.CatVar.LocBase;
        sz_vars;
        nb_all_tmps = 0;
        nb_all_refs = 0;
        sz_all_tmps = 0;
        nb_all_tables = 0;
        sz_all_tables = 0;
        max_nb_args = 0;
        table_map = IntMap.empty;
      }
  in
  (prog, stats)

let complete_target_vars ((prog : Validator.program), (stats : Mir.stats)) :
    Validator.program * Mir.stats =
  let fold _ (t : Validator.target) (prog_dict, max_nb_args) =
    let is_f = Com.target_is_function t in
    let prog_dict =
      match t.target_result with
      | Some m_id ->
          let var = IntMap.find (Pos.unmark m_id) prog_dict in
          let var = Com.Var.set_loc_idx var (-t.target_sz_tmps) in
          IntMap.add var.id var prog_dict
      | None -> prog_dict
    in
    let prog_dict, _ =
      let idx_init =
        if is_f then -t.target_sz_tmps + 1 else -t.target_nb_refs
      in
      List.fold_left
        (fun (prog_dict, n) m_id ->
          let var = IntMap.find (Pos.unmark m_id) prog_dict in
          let var = Com.Var.set_loc_idx var n in
          let prog_dict = IntMap.add var.id var prog_dict in
          (prog_dict, n + 1))
        (prog_dict, idx_init) t.target_args
    in
    let prog_dict, _ =
      let idx_init =
        let tmp_sz =
          StrMap.fold
            (fun _ m_id n ->
              let var = IntMap.find (Pos.unmark m_id) prog_dict in
              n + Com.Var.size var)
            t.target_tmp_vars 0
        in
        -tmp_sz
      in
      StrMap.fold
        (fun _name m_id (prog_dict, n) ->
          let var = IntMap.find (Pos.unmark m_id) prog_dict in
          let var = Com.Var.set_loc_idx var n in
          let prog_dict = IntMap.add var.id var prog_dict in
          (prog_dict, n + Com.Var.size var))
        t.target_tmp_vars (prog_dict, idx_init)
    in
    let prog_dict =
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
    let max_nb_args =
      if Com.target_is_function t then
        max max_nb_args (List.length t.target_args)
      else max_nb_args
    in
    (prog_dict, max_nb_args)
  in
  let prog_dict, max_nb_args =
    (prog.prog_dict, 0)
    |> StrMap.fold fold prog.prog_targets
    |> StrMap.fold fold prog.prog_functions
  in
  let prog = { prog with prog_dict } in
  let stats = { stats with max_nb_args } in
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
          let v = Com.Var.set_loc_tab_idx v (IntMap.cardinal map) in
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
              (t.target_nb_tmps, t.target_sz_tmps, t.target_nb_refs)
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
    and aux_access tdata m_a =
      match Pos.unmark m_a with
      | Com.VarAccess _ -> (0, 0, 0, tdata)
      | Com.TabAccess (_, _, mi) | Com.FieldAccess (_, mi, _, _) ->
          aux_expr tdata mi
    and aux_instr tdata (Pos.Mark (instr, _pos)) =
      match instr with
      | Com.Affectation mf -> (
          match Pos.unmark mf with
          | SingleFormula (VarDecl (m_a, mev)) ->
              let nb, sz, nbRef, tdata = aux_expr tdata mev in
              let nbA, szA, nbRefA, tdata = aux_access tdata m_a in
              (max nbA nb, max szA sz, max nbRefA nbRef, tdata)
          | SingleFormula (EventFieldRef (mei, _, _, _)) -> aux_expr tdata mei
          | MultipleFormulaes _ -> assert false)
      | Com.ComputeTarget (tn, al, _) ->
          let nbA, szA, nbRefA, tdata =
            let fold (nb, sz, nbRef, tdata) m_a =
              let nbA, szA, nbRefA, tdata = aux_access tdata m_a in
              let nb = max nb nbA in
              let sz = max sz szA in
              let nbRef = max nbRef nbRefA in
              (nb, sz, nbRef, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) al
          in
          let nbC, szC, nbRefC, tdata = aux_call tdata (Pos.unmark tn) in
          (max nbA nbC, max szA szC, max nbRefA nbRefC, tdata)
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
          let fold (nb, sz, nbRef, tdata) (Pos.Mark (a, _pos)) =
            match a with
            | Com.PrintAccess (_, m_a) ->
                let nbA, szA, nbRefA, tdata = aux_access tdata m_a in
                (max nbA nb, max szA sz, max nbRefA nbRef, tdata)
            | Com.PrintString _ -> (nb, sz, nbRef, tdata)
            | Com.PrintIndent me | Com.PrintExpr (me, _, _) ->
                let nb', sz', nbRef', tdata = aux_expr tdata me in
                (max nb nb', max sz sz', max nbRef nbRef', tdata)
          in
          List.fold_left fold (0, 0, 0, tdata) pal
      | Com.Iterate (_, al, mel, instrs) ->
          let nb0, sz0, nbRef0, tdata =
            let fold (nb, sz, nbRef, tdata) m_a =
              let nbA, szA, nbRefA, tdata = aux_access tdata m_a in
              let nb = max nb nbA in
              let sz = max sz szA in
              let nbRef = max nbRef nbRefA in
              (nb, sz, nbRef, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) al
          in
          let nb1, sz1, nbRef1, tdata =
            let fold (nb, sz, nbRef, tdata) (_, me, _) =
              let nbE, szE, nbRefE, tdata = aux_expr tdata me in
              (max nb nbE, max sz szE, max nbRef nbRefE, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) mel
          in
          let nb2, sz2, nbRef2, tdata = aux_instrs tdata instrs in
          let nb = max nb0 @@ max nb1 nb2 in
          let sz = max sz0 @@ max sz1 sz2 in
          let nbRef = 1 + (max nbRef0 @@ max nbRef1 nbRef2) in
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
      | Com.Restore (al, var_params, evts, evtfs, instrs) ->
          let nb0, sz0, nbRef0, tdata =
            let fold (nb, sz, nbRef, tdata) m_a =
              let nbA, szA, nbRefA, tdata = aux_access tdata m_a in
              let nb = max nb nbA in
              let sz = max sz szA in
              let nbRef = max nbRef nbRefA in
              (nb, sz, nbRef, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) al
          in
          let nb1, sz1, nbRef1, tdata =
            let fold (nb, sz, nbRef, tdata) (_, _, me, _) =
              let nbE, szE, nbRefE, tdata = aux_expr tdata me in
              (max nb nbE, max sz szE, max nbRef nbRefE, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) var_params
          in
          let nb2, sz2, nbRef2, tdata =
            let fold (nb, sz, nbRef, tdata) me =
              let nbE, szE, nbRefE, tdata = aux_expr tdata me in
              (max nb nbE, max sz szE, max nbRef nbRefE, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) evts
          in
          let nb3, sz3, nbRef3, tdata =
            let fold (nb, sz, nbRef, tdata) (_, me) =
              let nbE, szE, nbRefE, tdata = aux_expr tdata me in
              (max nb nbE, max sz szE, max nbRef nbRefE, tdata)
            in
            List.fold_left fold (0, 0, 0, tdata) evtfs
          in
          let nb4, sz4, nbRef4, tdata = aux_instrs tdata instrs in
          let nb = max nb0 @@ max nb1 @@ max nb2 @@ max nb3 nb4 in
          let sz = max sz0 @@ max sz1 @@ max sz2 @@ max sz3 sz4 in
          (* ??? *)
          let nbRef =
            1 + (max nbRef0 @@ max nbRef1 @@ max nbRef2 @@ max nbRef3 nbRef4)
          in
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
    and aux_expr tdata (Pos.Mark (expr, _pos)) =
      match expr with
      | Com.TestInSet (_, me, values) ->
          let fold (nb, sz, nbRef, tdata) = function
            | Com.VarValue (Pos.Mark (TabAccess (_, _, mei), _))
            | Com.VarValue (Pos.Mark (FieldAccess (_, mei, _, _), _)) ->
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
      | Com.Var (TabAccess (_, _, me))
      | Com.Var (FieldAccess (_, me, _, _))
      | Com.Size (Pos.Mark (TabAccess (_, _, me), _))
      | Com.Size (Pos.Mark (FieldAccess (_, me, _, _), _))
      | Com.Attribut (Pos.Mark (TabAccess (_, _, me), _), _)
      | Com.Attribut (Pos.Mark (FieldAccess (_, me, _, _), _), _)
      | Com.IsVariable (Pos.Mark (TabAccess (_, _, me), _), _)
      | Com.IsVariable (Pos.Mark (FieldAccess (_, me, _, _), _), _) ->
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
      | Com.NbCategory _ | Com.Attribut _ | Com.Size _ | Com.IsVariable _
      | Com.NbAnomalies | Com.NbDiscordances | Com.NbInformatives
      | Com.NbBloquantes ->
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

(** {1 Translation} *)

(** {2 General translation context} *)

let get_var (dict : Com.Var.t IntMap.t) (m_id : int Pos.marked) : Com.Var.t =
  IntMap.find (Pos.unmark m_id) dict

(** {2 Translation of expressions} *)

let rec translate_expression (p : Validator.program) (dict : Com.Var.t IntMap.t)
    (f : int Pos.marked Com.m_expression) : Mir.m_expression =
  let open Com in
  let expr =
    match Pos.unmark f with
    | TestInSet (positive, m_e, values) ->
        let new_e = translate_expression p dict m_e in
        let new_set_values =
          List.map
            (function
              | FloatValue f -> FloatValue f
              | VarValue (Pos.Mark (access, pos)) ->
                  let access' = translate_access p dict access in
                  VarValue (Pos.mark access' pos)
              | IntervalValue (bv, ev) -> IntervalValue (bv, ev))
            (values : int Pos.marked set_value list)
        in
        TestInSet (positive, new_e, new_set_values)
    | Comparison (op, e1, e2) ->
        let new_e1 = translate_expression p dict e1 in
        let new_e2 = translate_expression p dict e2 in
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
        let new_e1 = translate_expression p dict e1 in
        let new_e2 = translate_expression p dict e2 in
        Binop (op, new_e1, new_e2)
    | Unop (op, e) ->
        let new_e = translate_expression p dict e in
        Unop (op, new_e)
    | Conditional (e1, e2, e3) ->
        let new_e1 = translate_expression p dict e1 in
        let new_e2 = translate_expression p dict e2 in
        let new_e3 = Option.map (translate_expression p dict) e3 in
        Conditional (new_e1, new_e2, new_e3)
    | FuncCall (f_name, args) ->
        let new_args =
          List.map (fun arg -> translate_expression p dict arg) args
        in
        FuncCall (f_name, new_args)
    | Literal l -> Literal l
    | Var access -> Var (translate_access p dict access)
    | NbCategory cs -> NbCategory (Validator.mast_to_catvars cs p.prog_var_cats)
    | Attribut (Pos.Mark (access, pos), a) -> (
        match access with
        | VarAccess (_, m_id) -> (
            let var = get_var dict m_id in
            if Com.Var.is_ref var then
              let access' = translate_access p dict access in
              Attribut (Pos.mark access' pos, a)
            else
              match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
              | Some l -> Literal (Float (float (Pos.unmark l)))
              | None -> Literal Undefined)
        | TabAccess (_, m_id, _) -> (
            let var = get_var dict m_id in
            match StrMap.find_opt (Pos.unmark a) (Com.Var.attrs var) with
            | Some l -> Literal (Float (float (Pos.unmark l)))
            | None -> Literal Undefined)
        | FieldAccess (m_sp_opt, e, f, _) ->
            let m_sp_opt' =
              Option.map
                (fun (m_sp, _) ->
                  let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
                  let i_sp = StrMap.find sp_name p.prog_var_spaces in
                  (m_sp, i_sp))
                m_sp_opt
            in
            let e' = translate_expression p dict e in
            let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
            Attribut (Pos.mark (FieldAccess (m_sp_opt', e', f, i)) pos, a))
    | Size (Pos.Mark (access, pos)) -> (
        match access with
        | VarAccess (_, m_id) ->
            let var = get_var dict m_id in
            if Com.Var.is_ref var then
              let access' = translate_access p dict access in
              Size (Pos.mark access' pos)
            else Literal (Float (float @@ Com.Var.size var))
        | TabAccess _ -> Literal (Float 1.0)
        | FieldAccess (m_sp_opt, e, f, _) ->
            let m_sp_opt' =
              Option.map
                (fun (m_sp, _) ->
                  let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
                  let i_sp = StrMap.find sp_name p.prog_var_spaces in
                  (m_sp, i_sp))
                m_sp_opt
            in
            let e' = translate_expression p dict e in
            let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
            Size (Pos.mark (FieldAccess (m_sp_opt', e', f, i)) pos))
    | IsVariable (Pos.Mark (access, pos), m_name) -> (
        match access with
        | VarAccess (_, m_id) -> (
            let var = get_var dict m_id in
            if Com.Var.is_ref var then
              let access' = translate_access p dict access in
              IsVariable (Pos.mark access' pos, m_name)
            else
              let name = Pos.unmark m_name in
              if Com.Var.name_str var = name then Literal (Float 1.0)
              else
                match Com.Var.alias var with
                | Some m_a when Pos.unmark m_a = name -> Literal (Float 1.0)
                | _ -> Literal (Float 0.0))
        | _ ->
            let access' = translate_access p dict access in
            IsVariable (Pos.mark access' pos, m_name))
    | NbAnomalies -> NbAnomalies
    | NbDiscordances -> NbDiscordances
    | NbInformatives -> NbInformatives
    | NbBloquantes -> NbBloquantes
    | FuncCallLoop _ | Loop _ -> assert false
  in
  Pos.same expr f

and translate_access (p : Validator.program) (dict : Com.Var.t IntMap.t)
    (access : int Pos.marked Com.access) : Com.Var.t Com.access =
  let trans_m_sp_opt m_sp_opt =
    Option.map
      (fun (m_sp, _) ->
        let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
        let i_sp = StrMap.find sp_name p.prog_var_spaces in
        (m_sp, i_sp))
      m_sp_opt
  in
  match access with
  | VarAccess (m_sp_opt, m_v) ->
      let m_sp_opt' = trans_m_sp_opt m_sp_opt in
      let v' = get_var dict m_v in
      Com.VarAccess (m_sp_opt', v')
  | TabAccess (m_sp_opt, m_v, m_i) ->
      let m_sp_opt' = trans_m_sp_opt m_sp_opt in
      let v' = get_var dict m_v in
      let m_i' = translate_expression p dict m_i in
      Com.TabAccess (m_sp_opt', v', m_i')
  | FieldAccess (m_sp_opt, i, f, _) ->
      let m_sp_opt' = trans_m_sp_opt m_sp_opt in
      let i' = translate_expression p dict i in
      let ef = StrMap.find (Pos.unmark f) p.prog_event_fields in
      Com.FieldAccess (m_sp_opt', i', f, ef.index)

(** {2 Translation of instructions} *)

let rec translate_prog (p : Validator.program) (dict : Com.Var.t IntMap.t)
    (it_depth : int) (itval_depth : int) prog =
  let rev_fst (l, d) = (List.rev l, d) in
  let rec aux (res, dict) = function
    | [] -> (List.rev res, dict)
    | Pos.Mark (Com.Affectation (Pos.Mark (SingleFormula decl, _)), pos) :: il
      ->
        let decl' =
          match decl with
          | VarDecl (m_access, e) ->
              let access' = translate_access p dict (Pos.unmark m_access) in
              let m_access' = Pos.same access' m_access in
              let e' = translate_expression p dict e in
              Com.VarDecl (m_access', e')
          | EventFieldRef (idx, f, _, m_v) ->
              let idx' = translate_expression p dict idx in
              let i = (StrMap.find (Pos.unmark f) p.prog_event_fields).index in
              let v' = get_var dict m_v in
              Com.EventFieldRef (idx', f, i, v')
        in
        let m_form = Pos.mark (Com.SingleFormula decl') pos in
        aux (Pos.mark (Com.Affectation m_form) pos :: res, dict) il
    | Pos.Mark (Com.Affectation (Pos.Mark (MultipleFormulaes _, _)), _) :: _ ->
        assert false
    | Pos.Mark (Com.IfThenElse (e, ilt, ile), pos) :: il ->
        let expr = translate_expression p dict e in
        let prog_then, dict = aux ([], dict) ilt in
        let prog_else, dict = aux ([], dict) ile in
        let instr' = Com.IfThenElse (expr, prog_then, prog_else) in
        aux (Pos.mark instr' pos :: res, dict) il
    | Pos.Mark (Com.WhenDoElse (wdl, ed), pos) :: il ->
        let wdl', dict =
          rev_fst
          @@ List.fold_left
               (fun (res, dict) (expr, dl, pos) ->
                 let expr' = translate_expression p dict expr in
                 let dl', dict = aux ([], dict) dl in
                 ((expr', dl', pos) :: res, dict))
               ([], dict) wdl
        in
        let ed', dict = aux ([], dict) (Pos.unmark ed) in
        let ed' = Pos.same ed' ed in
        aux (Pos.mark (Com.WhenDoElse (wdl', ed')) pos :: res, dict) il
    | Pos.Mark (Com.ComputeTarget (tn, targs, m_sp_opt), pos) :: il ->
        let targs' = List.map (Pos.map (translate_access p dict)) targs in
        let m_sp_opt' =
          Option.map
            (fun (m_sp, _) ->
              let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
              let i_sp = StrMap.find sp_name p.prog_var_spaces in
              (m_sp, i_sp))
            m_sp_opt
        in
        let instr' = Com.ComputeTarget (tn, targs', m_sp_opt') in
        aux (Pos.mark instr' pos :: res, dict) il
    | Pos.Mark (Com.VerifBlock instrs, pos) :: il ->
        let instrs', dict = aux ([], dict) instrs in
        aux (Pos.mark (Com.VerifBlock instrs') pos :: res, dict) il
    | Pos.Mark (Com.Print (std, args), pos) :: il ->
        let mir_args =
          List.rev
            (List.fold_left
               (fun res arg ->
                 let mir_arg =
                   match Pos.unmark arg with
                   | Com.PrintString s -> Com.PrintString s
                   | Com.PrintAccess (info, m_a) ->
                       let a' = translate_access p dict (Pos.unmark m_a) in
                       let m_a' = Pos.same a' m_a in
                       Com.PrintAccess (info, m_a')
                   | Com.PrintIndent e ->
                       Com.PrintIndent (translate_expression p dict e)
                   | Com.PrintExpr (e, min, max) ->
                       Com.PrintExpr (translate_expression p dict e, min, max)
                 in
                 Pos.same mir_arg arg :: res)
               [] args)
        in
        aux (Pos.mark (Com.Print (std, mir_args)) pos :: res, dict) il
    | Pos.Mark (Com.Iterate (m_id, al, var_params, instrs), pos) :: il ->
        let var = get_var dict m_id in
        let var = Com.Var.set_loc_idx var it_depth in
        let dict = IntMap.add var.id var dict in
        let al' = List.map (Pos.map (translate_access p dict)) al in
        let var_params' =
          List.map
            (fun (vcats, expr, m_sp_opt) ->
              let catSet = Validator.mast_to_catvars vcats p.prog_var_cats in
              let mir_expr = translate_expression p dict expr in
              let m_sp_opt' =
                Option.map
                  (fun (m_sp, _) ->
                    let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
                    let i_sp = StrMap.find sp_name p.prog_var_spaces in
                    (m_sp, i_sp))
                  m_sp_opt
              in
              (catSet, mir_expr, m_sp_opt'))
            var_params
        in
        let prog_it, dict =
          translate_prog p dict (it_depth + 1) itval_depth instrs
        in
        let instr = Com.Iterate (var, al', var_params', prog_it) in
        aux (Pos.mark instr pos :: res, dict) il
    | Pos.Mark (Com.Iterate_values (m_id, var_intervals, instrs), pos) :: il ->
        let var = get_var dict m_id in
        let var = Com.Var.set_loc_idx var itval_depth in
        let dict = IntMap.add var.id var dict in
        let var_intervals' =
          List.map
            (fun (e0, e1, step) ->
              let e0' = translate_expression p dict e0 in
              let e1' = translate_expression p dict e1 in
              let step' = translate_expression p dict step in
              (e0', e1', step'))
            var_intervals
        in
        let prog_it, dict =
          translate_prog p dict it_depth (itval_depth + 1) instrs
        in
        let instr = Com.Iterate_values (var, var_intervals', prog_it) in
        aux (Pos.mark instr pos :: res, dict) il
    | Pos.Mark (Com.Restore (al, var_params, evts, evtfs, instrs), pos) :: il ->
        let al' = List.map (Pos.map (translate_access p dict)) al in
        let var_params', dict =
          let var_params', dict =
            List.fold_left
              (fun (res, dict) (m_id, vcats, expr, m_sp_opt) ->
                let var = get_var dict m_id in
                let var = Com.Var.set_loc_idx var it_depth in
                let dict = IntMap.add var.id var dict in
                let catSet = Validator.mast_to_catvars vcats p.prog_var_cats in
                let mir_expr = translate_expression p dict expr in
                let m_sp_opt' =
                  Option.map
                    (fun (m_sp, _) ->
                      let sp_name = Com.get_normal_var @@ Pos.unmark m_sp in
                      let i_sp = StrMap.find sp_name p.prog_var_spaces in
                      (m_sp, i_sp))
                    m_sp_opt
                in
                ((var, catSet, mir_expr, m_sp_opt') :: res, dict))
              ([], dict) var_params
          in
          (List.rev var_params', dict)
        in
        let evts' = List.map (translate_expression p dict) evts in
        let evtfs', dict =
          rev_fst
          @@ List.fold_left
               (fun (res, dict) (m_id, expr) ->
                 let var = get_var dict m_id in
                 let var = Com.Var.set_loc_idx var itval_depth in
                 let dict = IntMap.add var.id var dict in
                 let mir_expr = translate_expression p dict expr in
                 ((var, mir_expr) :: res, dict))
               ([], dict) evtfs
        in
        let prog_rest, dict =
          translate_prog p dict it_depth itval_depth instrs
        in
        let instr = Com.Restore (al', var_params', evts', evtfs', prog_rest) in
        aux (Pos.mark instr pos :: res, dict) il
    | Pos.Mark (Com.ArrangeEvents (sort, filter, add, instrs), pos) :: il ->
        let sort', itval_depth', dict =
          match sort with
          | Some (m_id0, m_id1, expr) ->
              let var0 = get_var dict m_id0 in
              let var0' = Com.Var.set_loc_idx var0 itval_depth in
              let var1 = get_var dict m_id1 in
              let var1' = Com.Var.set_loc_idx var1 (itval_depth + 1) in
              let dict =
                dict |> IntMap.add var0.id var0' |> IntMap.add var1.id var1'
              in
              let expr' = translate_expression p dict expr in
              (Some (var0', var1', expr'), itval_depth + 2, dict)
          | None -> (None, itval_depth, dict)
        in
        let filter', itval_depth', dict =
          match filter with
          | Some (m_id, expr) ->
              let var = get_var dict m_id in
              let var' = Com.Var.set_loc_idx var itval_depth in
              let dict = IntMap.add var.id var' dict in
              let expr' = translate_expression p dict expr in
              (Some (var', expr'), max itval_depth' (itval_depth + 1), dict)
          | None -> (None, itval_depth', dict)
        in
        let add' = Option.map (translate_expression p dict) add in
        let instrs', dict =
          translate_prog p dict it_depth itval_depth' instrs
        in
        let instr = Com.ArrangeEvents (sort', filter', add', instrs') in
        aux (Pos.mark instr pos :: res, dict) il
    | Pos.Mark (Com.RaiseError (err_name, var_opt), pos) :: il ->
        let err_decl = StrMap.find (Pos.unmark err_name) p.prog_errors in
        let m_err_decl = Pos.same err_decl err_name in
        let instr' = Com.RaiseError (m_err_decl, var_opt) in
        aux (Pos.mark instr' pos :: res, dict) il
    | Pos.Mark (Com.CleanErrors, pos) :: il ->
        aux (Pos.mark Com.CleanErrors pos :: res, dict) il
    | Pos.Mark (Com.ExportErrors, pos) :: il ->
        aux (Pos.mark Com.ExportErrors pos :: res, dict) il
    | Pos.Mark (Com.FinalizeErrors, pos) :: il ->
        aux (Pos.mark Com.FinalizeErrors pos :: res, dict) il
    | Pos.Mark (Com.ComputeDomain _, _) :: _
    | Pos.Mark (Com.ComputeChaining _, _) :: _
    | Pos.Mark (Com.ComputeVerifs _, _) :: _ ->
        assert false
  in
  aux ([], dict) prog

let get_targets (p : Validator.program) (dict : Com.Var.t IntMap.t)
    (ts : Validator.target StrMap.t) : Mir.target StrMap.t * Com.Var.t IntMap.t
    =
  StrMap.fold
    (fun _ (t : Validator.target) (targets, dict) ->
      let is_f = Com.target_is_function t in
      let target_name = t.target_name in
      let target_file = t.target_file in
      let target_apps = t.target_apps in
      let target_nb_tmps = t.target_nb_tmps in
      let target_nb_refs = t.target_nb_refs in
      let target_args =
        let map m_id = IntMap.find (Pos.unmark m_id) dict in
        List.map map t.target_args
      in
      let target_sz_tmps = t.target_sz_tmps in
      let target_result =
        let map m_id = IntMap.find (Pos.unmark m_id) dict in
        Option.map map t.target_result
      in
      let target_tmp_vars =
        let map m_id = IntMap.find (Pos.unmark m_id) dict in
        StrMap.map map t.target_tmp_vars
      in
      let nb_args = List.length target_args in
      let ref_depth = -target_nb_refs + if is_f then 0 else nb_args in
      let itval_depth = -target_sz_tmps + if is_f then 1 + nb_args else 0 in
      let target_prog, dict =
        translate_prog p dict ref_depth itval_depth t.target_prog
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
      (StrMap.add (Pos.unmark target_name) target targets, dict))
    ts (StrMap.empty, dict)

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
  let dict = p.prog_dict in
  let program_functions, dict = get_targets p dict p.prog_functions in
  let program_targets, dict = get_targets p dict p.prog_targets in
  let program_dict = dict in
  let program_var_space_def =
    let id = StrMap.find "" p.prog_var_spaces in
    IntMap.find id p.prog_var_spaces_idx
  in
  Mir.
    {
      program_safe_prefix = p.prog_prefix;
      program_applications = p.prog_apps;
      program_var_categories = p.prog_var_cats;
      program_rule_domains = p.prog_rdoms;
      program_verif_domains = p.prog_vdoms;
      program_dict;
      program_vars;
      program_alias;
      program_var_spaces = p.prog_var_spaces;
      program_var_spaces_idx = p.prog_var_spaces_idx;
      program_var_space_def;
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
