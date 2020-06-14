(* Copyright (C) 2020 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(** This modules define the logic used to call the M code several times in order to compute the
    amount of tax correctly *)

open Mvg

let var_is_ (attr : string) (v : Variable.t) : bool =
  List.exists
    (fun ((attr_name, _), (attr_value, _)) -> attr_name = attr && attr_value = Ast.Float 1.)
    v.Mvg.Variable.attributes

let var_is_deposit = var_is_ "acompte"

let var_is_taxbenefit = var_is_ "avfisc"

let var_is_base = var_is_ "base"

let var_is_computed = var_is_ "calculee"

let var_is_revenue_but_not_deposit (v : Variable.t) : bool =
  List.for_all
    (fun ((attr_name, _), (attr_value, _)) ->
      not (attr_name = "acompte" && attr_value = Ast.Float 1.))
    v.Mvg.Variable.attributes
  && v.Variable.is_income

let is_undefined = function Undefined -> true | _ -> false

(** Equivalent to AC_IsCalculAcptes in the DGFiP's logic *)
let exists_deposit_defined_variables (input_values : literal VariableMap.t) : bool =
  VariableMap.exists
    (fun v _ -> (not (var_is_deposit v)) && not (is_undefined (VariableMap.find v input_values)))
    input_values

(** Equivalent to AC_IsCalculAvFisc in the DGFiP's codebase *)
let exists_taxbenefit_defined_variables (input_values : literal VariableMap.t) : bool =
  VariableMap.exists (fun k _ -> var_is_taxbenefit k) input_values

(** Equivalent to AC_IsCodeSuppAvFisc in the DGFiP's codebase *)
let exists_taxbenefit_ceiled_variables (input_values : literal VariableMap.t) : bool =
  (* seems like checking avifsc = 2 isn't sufficient... *)
  let supp_avfisc =
    [
      "7QK";
      "7QD";
      "7QB";
      "7QC";
      "4BA";
      "4BY";
      "4BB";
      "4BC";
      "7CL";
      "7CM";
      "7CN";
      "7QE";
      "7QF";
      "7QG";
      "7QH";
      "7QI";
      "7QJ";
      "7LG";
      "7MA";
      "7QM";
      "2DC";
      "7KM";
      "7KG";
      "7QP";
      "7QS";
      "7QN";
      "7QO";
      "7QL";
      "7LS";
    ]
  in
  VariableMap.exists
    (fun var _ -> match var.alias with None -> false | Some a -> List.mem a supp_avfisc)
    input_values

(** Equivalent to AC_GetCodesAvFisc in the DGFiP's logic *)
let all_deposit_defined_variables (input_values : literal VariableMap.t) : unit VariableMap.t =
  VariableMap.map (fun _ -> ()) (VariableMap.filter (fun v _ -> var_is_deposit v) input_values)

(** Equivalent to AC_GetCodesAcompte in the DGFiP's logic *)
let all_revenues_defined_not_deposit (input_values : literal VariableMap.t) : unit VariableMap.t =
  VariableMap.map
    (fun _ -> ())
    (VariableMap.filter (fun v _ -> var_is_revenue_but_not_deposit v) input_values)

let find_var_by_name p name =
  try
    let vars =
      Pos.VarNameToID.find name p.Mvg.program_idmap
      |> List.sort (fun v1 v2 ->
             compare_execution_number v1.Variable.execution_number v2.Variable.execution_number)
    in
    List.hd vars
  with Not_found -> (
    try
      let name = find_var_name_by_alias p name in
      List.hd
        (List.sort
           (fun v1 v2 -> compare v1.Mvg.Variable.execution_number v2.Mvg.Variable.execution_number)
           (Pos.VarNameToID.find name p.program_idmap))
    with Not_found ->
      Cli.debug_print "not found: %s@." name;
      raise Not_found )

(* FIXME *)

(** Equivalent to IRDATA_range *)
let update_ctx_var p var value (ctx : Interpreter.ctx) : Interpreter.ctx =
  let var = find_var_by_name p var in
  {
    ctx with
    ctx_vars = Mvg.VariableMap.add var (Interpreter.SimpleVar (Mvg.Float value)) ctx.ctx_vars;
  }

(** Equivalent to IRDATA_get_var_irdata *)
let get_ctx_var p (ctx : Interpreter.ctx) (var : string) : Interpreter.var_literal =
  let var = find_var_by_name p var in
  (* FIXME: if not found, check somewhere else? *)
  try Mvg.VariableMap.find var ctx.ctx_vars
  with Not_found ->
    Cli.debug_print "get_ctx_var %a failed@." Format_mvg.format_variable var;
    raise Not_found

(** Equivalent to IRDATA_efface *)
let clear_ctx_var p (ctx : Interpreter.ctx) var =
  let var = find_var_by_name p var in
  (* FIXME: remove or set to undefined? *)
  {
    ctx with
    ctx_vars =
      (*Mlang.Mvg.VariableMap.remove var ctx.ctx_vars*)
      Mvg.VariableMap.add var (Interpreter.SimpleVar Mvg.Undefined) ctx.ctx_vars;
  }

let partition_ctx choice_function (ctx : Interpreter.ctx) =
  let ctx_vars = ctx.ctx_vars in
  let ctx_vars_t, ctx_vars_f =
    Mvg.VariableMap.fold
      (fun var value (t, f) ->
        let corresponding_undefined =
          match value with
          | Interpreter.SimpleVar _ -> Interpreter.SimpleVar Undefined
          | Interpreter.TableVar (size, _) -> Interpreter.TableVar (size, Array.make size Undefined)
        in
        if choice_function var then
          (VariableMap.add var value t, VariableMap.add var corresponding_undefined f)
        else (VariableMap.add var corresponding_undefined t, VariableMap.add var value t))
      ctx_vars
      (VariableMap.empty, VariableMap.empty)
    (* Mvg.VariableMap.partition (fun var _ -> choice_function var) ctx_vars *)
  in
  ({ ctx with ctx_vars = ctx_vars_t }, { ctx with ctx_vars = ctx_vars_f })

let merge_ctx (ctx1 : Interpreter.ctx) (ctx2 : Interpreter.ctx) =
  let ctx1_vars = ctx1.ctx_vars in
  let ctx2_vars = ctx2.ctx_vars in
  let ctx_vars =
    Mvg.VariableMap.merge
      (fun _ ov ov' ->
        match (ov, ov') with Some v, None | _, Some v -> Some v | _ -> assert false)
      ctx1_vars ctx2_vars
  in
  { ctx1 with ctx_vars }

let fabs x = if x >= 0. then x else -.x

let reset_ f (ctx : Interpreter.ctx) =
  let ctx_vars = ctx.ctx_vars in
  let ctx_vars =
    VariableMap.mapi
      (fun var value -> if f var then Interpreter.SimpleVar Mvg.Undefined else value)
      ctx_vars
  in
  { ctx with ctx_vars }

let reset_calculee = reset_ var_is_computed

let reset_base = reset_ var_is_base

let extract_value (default : float) (v : Interpreter.var_literal) =
  match v with
  | SimpleVar (Mvg.Float f) -> f
  | SimpleVar (Mvg.Bool b) -> if b then 1. else 0.
  | _ -> default

(** Equivalent to AC_CalculeAvFiscal *)
let compute_benefit deps exec_order inputs npasses p ctx =
  Cli.debug_print "beginning compute_benefit@.";
  let ctx, ctx_others = partition_ctx var_is_taxbenefit ctx in
  (* FIXME: unsure about the while *)
  let besoincalcul = exists_taxbenefit_ceiled_variables inputs in
  let update_ctx = update_ctx_var p in
  let avantagefisc, ctx =
    if besoincalcul then (
      let () = Cli.debug_print "besoin calcul avfisc@." in
      let ctx = update_ctx "V_INDTEO" 1. ctx |> update_ctx "V_CALCUL_NAPS" 1. in
      let ctx, _ = Interpreter.evaluate_program_once deps exec_order inputs npasses (ctx, p) in

      let ctx = update_ctx "V_CALCUL_NAPS" 0. ctx in
      (* next access fails... woops *)
      let avantagefisc = extract_value 0. (get_ctx_var p ctx "NAPSANSPENA") in
      let iad11 = extract_value 0. (get_ctx_var p ctx "IAD11") in
      let ine = extract_value 0. (get_ctx_var p ctx "INE") in
      let ire = extract_value 0. (get_ctx_var p ctx "IRE") in
      let prem = extract_value 0. (get_ctx_var p ctx "PREM8_11") in
      Cli.debug_print "NAPSANSPENA=%f@.IAD11=%f@.INE=%f@.IRE=%f@.PREM8_11=%f@." avantagefisc iad11
        ine ire prem;
      (* Interpreter.repl_debugguer ctx p; *)
      let ctx = reset_calculee ctx in
      let ctx = reset_base ctx in
      let ctx = update_ctx "PREM8_11" prem ctx in
      (* FIXME: while loop to clean what? *)
      let ctx = merge_ctx ctx ctx_others in
      ( avantagefisc,
        ctx |> update_ctx "V_IAD11TEO" iad11 |> update_ctx "V_IRETEO" ire
        |> update_ctx "V_INETEO" ine ) )
    else (0., ctx)
  in
  Cli.debug_print "ending compute_benefit, avantages fiscaux = %f@." avantagefisc;
  (avantagefisc, ctx)

(** Equivalent to AC_CalculAcomptes *)
let compute_deposit deps exec_order inputs npasses p ctx =
  Cli.debug_print "beginning compute_deposit@.";
  let update_ctx = update_ctx_var p in
  let ctx = ctx |> update_ctx "FLAG_ACO" 1. |> update_ctx "V_CALCUL_ACO" 1. in
  let ctx, _ = Interpreter.evaluate_program_once deps exec_order inputs npasses (ctx, p) in
  let ctx = ctx |> update_ctx "V_CALCUL_ACO" 0. |> update_ctx "FLAG_ACO" 2. in
  let acompte = extract_value 0. (get_ctx_var p ctx "MTAP") in
  let prem = extract_value 0. (get_ctx_var p ctx "PREM8_11") in
  let ctx = reset_calculee ctx in
  let ctx = reset_base ctx in
  Cli.debug_print "ending compute_deposit@.";
  (acompte, update_ctx "PREM8_11" prem ctx)

(** Equivalent to AC_CalculeAcomptesAvFisc *)
let compute_deposit_with_benefit deps exec_order inputs npasses p ctx napsanpenareel =
  Cli.debug_print "beginning compute_deposit_with_benefit@.";
  let update_ctx = update_ctx_var p in
  let ctx = update_ctx "FLAG_ACO" 1. ctx in
  let montantavtmp, ctx = compute_benefit deps exec_order inputs npasses p ctx in
  Cli.debug_print "NAPTEO: %f@." montantavtmp;
  let ctx =
    update_ctx "V_INDTEO" 0. ctx
    |> update_ctx "V_NEGREEL" (if napsanpenareel > 0. then 0. else 1.)
    |> update_ctx "V_NAPREEL" (fabs napsanpenareel)
    |> update_ctx "V_CALCUL_ACO" 1.0
  in
  let ctx, _ = Interpreter.evaluate_program_once deps exec_order inputs npasses (ctx, p) in
  let ctx = update_ctx "V_CALCUL_ACO" 0. ctx |> update_ctx "FLAG_ACO" 2. in
  let acompte = extract_value 0. (get_ctx_var p ctx "MTAP") in
  let acompteps = extract_value 0. (get_ctx_var p ctx "MTAPPS") in
  let prem = extract_value 0. (get_ctx_var p ctx "PREM8_11") in
  let ctx = reset_calculee ctx in
  let ctx = update_ctx "V_ACO_MTAPPS" acompteps ctx |> update_ctx "PREM8_11" prem in
  Cli.debug_print "ending compute_deposit_with_benefit@.";
  (acompte, ctx)

(** Equivalent to IN_traite_double_liquidation3 in DGFiP's codebase *)
let compute_program (p : program) (t : Typechecker.typ_info) (inputs : literal VariableMap.t)
    (npasses : int) : Interpreter.ctx * program =
  try
    let deps = Dependency.create_dependency_graph p in
    let exec_order = Execution_order.get_execution_order p in
    let update_ctx = update_ctx_var p in
    let ctx =
      Interpreter.empty_ctx p t |> update_ctx "FLAG_ACO" 0. |> update_ctx "V_NEGACO" 0.
      |> update_ctx "V_AVFISCOPBIS" 0. |> update_ctx "V_DIFTEOREEL" 0. |> update_ctx "PREM8_11" 0.
    in
    (* do we need to perform "acomptes" computation? *)
    let calcul_acomptes = exists_deposit_defined_variables inputs in
    (* do we need to perform "avantages fiscal" computation? *)
    let calcul_avfisc = exists_taxbenefit_defined_variables inputs in
    let ctx, v_8ZG =
      if calcul_avfisc then
        match get_ctx_var p ctx "8ZG" with
        (* FIXME: check that NULL is indéfini the get_var_irdate. Check TYPE_REVENU too *)
        | SimpleVar (Mvg.Float f) -> (clear_ctx_var p ctx "8ZG", f)
        | SimpleVar (Mvg.Bool b) -> (clear_ctx_var p ctx "8ZG", if b then 1. else 0.)
        | _ -> (ctx, 0.)
      else (ctx, 0.)
    in
    Cli.debug_print "Début du calcul des acomptes";
    let ctx, acompte, indice_aco =
      if calcul_acomptes && false (* according to the call?*)
                                  (* and p_IsCalculAcomptes? *) then
        let ctx_deposit_only, ctx_others = partition_ctx var_is_deposit ctx in
        let acompte, ctx_deposit_only =
          if calcul_avfisc then
            compute_deposit_with_benefit deps exec_order inputs npasses p ctx_deposit_only 0.
            (* FIXME: optimize since last param seems to always be 0 *)
          else compute_deposit deps exec_order inputs npasses p ctx_deposit_only
        in
        let indice_aco = if acompte >= 0. then 0. else 1. in
        (merge_ctx ctx_deposit_only ctx_others, fabs acompte, indice_aco)
        (* FIXME: maybe we can merge ctx_deposit_only with ctx and that works? *)
        (* FIXME: l_besoincalculacptes = 0; lmontantreel = 0 *)
      else (ctx, -1., 1.)
    in
    Cli.debug_print "Fin du calcul des acomptes@.Début du calcul de plafonnement@.";
    let ctx =
      if calcul_avfisc then
        let ctx =
          ctx |> update_ctx "V_AVFISCOPBIS" 0. |> update_ctx "V_DIFTEOREEL" 0.
          |> update_ctx "V_INDTEO" 1.
        in
        let _, ctx = compute_benefit deps exec_order inputs npasses p ctx in
        ctx |> update_ctx "V_INDTEO" 0. |> update_ctx "V_NEGREEL" 1. |> update_ctx "V_NAPREEL" 0.
        (* FIXME: normalement c'est l_montantreel mais je pense qu'après 2018 il n'est plus assigné *)
      else ctx
    in
    let ctx = update_ctx "8ZG" v_8ZG ctx in
    Cli.debug_print "Valorisation de l'acompte %f, V_INDTEO=%f" acompte
      (extract_value (-10.) (get_ctx_var p ctx "V_INDTEO"));
    let ctx =
      (* to check: conditional unecessary due to default values? *)
      if acompte > 0. then
        ctx |> update_ctx "V_ACO_MTAP" acompte |> update_ctx "V_NEGACO" indice_aco
      else ctx |> update_ctx "V_ACO_MTAP" 0. |> update_ctx "V_NEGACO" 0.
    in
    let ctx, _ = Interpreter.evaluate_program_once deps exec_order inputs npasses (ctx, p) in
    (ctx, p)
  with Interpreter.RuntimeError (e, ctx) ->
    if !Interpreter.exit_on_rte then begin
      Cli.error_print "%a@?" Interpreter.format_runtime_error e;
      flush_all ();
      flush_all ();
      if !Interpreter.repl_debug then Interpreter.repl_debugguer ctx p;
      exit 1
    end
    else raise (Interpreter.RuntimeError (e, ctx))
