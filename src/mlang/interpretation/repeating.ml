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

let reset_and_add_outputs (p : Interpreter.interpretable_program) (outputs : string list) :
    Interpreter.interpretable_program =
  let outputs = List.map (fun out -> find_var_by_name p.ip_program out) outputs in
  let program =
    {
      p.ip_program with
      program_vars =
        VariableMap.mapi
          (fun var data ->
            if List.mem var outputs then
              match data.Mvg.var_io with
              | Input ->
                  raise
                    (Interpreter.RuntimeError
                       ( Interpreter.IncorrectOutputVariable
                           (Format.asprintf "%a is an input" Format_mvg.format_variable var),
                         Interpreter.empty_ctx p.ip_program ))
              | Output -> data
              | Regular -> { data with var_io = Output }
            else
              match data.Mvg.var_io with
              | Input | Regular -> data
              | Output -> { data with var_io = Regular })
          p.ip_program.program_vars;
    }
  in
  { p with ip_program = program }

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

(** Equivalent to IRDATA_range *)
let update_inputs_var (p : program) (var : string) (value : float) (inputs : literal VariableMap.t)
    : literal VariableMap.t =
  let var = find_var_by_name p var in
  Mvg.VariableMap.add var (Mvg.Float value) inputs

(** Equivalent to IRDATA_get_var_irdata *)
let get_inputs_var (p : program) (var : string) (inputs : literal VariableMap.t) : literal =
  let var = find_var_by_name p var in
  match Mvg.VariableMap.find_opt var inputs with Some v -> v | None -> Undefined

(** Equivalent to IRDATA_get_var_irdata *)
let get_ctx_var p (ctx : Interpreter.ctx) (var : string) : Interpreter.var_literal =
  let var = find_var_by_name p var in
  (* FIXME: if not found, check somewhere else? *)
  try Mvg.VariableMap.find var ctx.ctx_vars
  with Not_found ->
    Cli.debug_print "get_ctx_var %a failed@." Format_mvg.format_variable var;
    raise Not_found

let get_input_var p inputs var : Interpreter.var_literal =
  let var = find_var_by_name p var in
  try SimpleVar (Mvg.VariableMap.find var inputs)
  with Not_found -> (* different than usual *)
                    SimpleVar Undefined

(** Equivalent to IRDATA_efface *)
let clear_inputs_var (p : program) (inputs : literal VariableMap.t) (var : string) :
    literal VariableMap.t =
  let var = find_var_by_name p var in
  VariableMap.remove var inputs

let partition_inputs (in_first_map : Variable.t -> bool) (inputs : literal VariableMap.t) :
    literal VariableMap.t * literal VariableMap.t =
  Mvg.VariableMap.partition (fun var _ -> not (in_first_map var)) inputs

let merge_inputs (modified : literal VariableMap.t) (others : literal VariableMap.t) :
    literal VariableMap.t =
  Mvg.VariableMap.merge
    (fun _ v1 v2 ->
      match (v1, v2) with
      | Some v1, None -> Some v1
      | None, Some v2 -> Some v2
      | None, None -> None
      | Some v1, Some _v2 -> Some v1)
    modified others

let fabs x = if x >= 0. then x else -.x

let extract_value (default : float) (v : Interpreter.var_literal) =
  match v with SimpleVar (Mvg.Float f) -> f | _ -> default

(** Equivalent to AC_CalculeAvFiscal *)
let compute_benefit (p : Interpreter.interpretable_program) (inputs : literal VariableMap.t) :
    float * literal VariableMap.t =
  Cli.debug_print "beginning compute_benefit@.";
  let besoincalcul =
    VariableMap.cardinal (all_deposit_defined_variables inputs) > 0
    || exists_taxbenefit_ceiled_variables inputs
  in
  let inputs, inputs_other = partition_inputs var_is_taxbenefit inputs in
  let update_inputs = update_inputs_var p.ip_program in
  let avantagefisc, inputs =
    if besoincalcul then begin
      let () = Cli.debug_print "besoin calcul avfisc@." in
      let inputs = inputs |> update_inputs "V_INDTEO" 1. |> update_inputs "V_CALCUL_NAPS" 1. in
      let p = reset_and_add_outputs p [ "NAPSANSPENA"; "IAD11"; "INE"; "IRE"; "PREM8_11" ] in
      let ctx = Interpreter.evaluate_program p inputs false in
      let inputs = update_inputs "V_CALCUL_NAPS" 0. inputs in
      let avantagefisc = extract_value 0. (get_ctx_var p.ip_program ctx "NAPSANSPENA") in
      let iad11 = extract_value 0. (get_ctx_var p.ip_program ctx "IAD11") in
      let ine = extract_value 0. (get_ctx_var p.ip_program ctx "INE") in
      let ire = extract_value 0. (get_ctx_var p.ip_program ctx "IRE") in
      let prem = extract_value 0. (get_ctx_var p.ip_program ctx "PREM8_11") in
      Cli.debug_print "NAPSANSPENA=%f@.IAD11=%f@.INE=%f@.IRE=%f@.PREM8_11=%f@." avantagefisc iad11
        ine ire prem;
      let inputs = update_inputs "PREM8_11" prem inputs in
      let inputs = merge_inputs inputs inputs_other in
      let inputs =
        inputs |> update_inputs "V_IAD11TEO" iad11 |> update_inputs "V_IRETEO" ire
        |> update_inputs "V_INETEO" ine
      in
      (avantagefisc, inputs)
    end
    else (0., merge_inputs inputs inputs_other)
  in
  Cli.debug_print "ending compute_benefit, avantages fiscaux = %f@." avantagefisc;
  (avantagefisc, inputs)

(** Equivalent to AC_CalculAcomptes *)
let compute_deposit (p : Interpreter.interpretable_program) (inputs : literal VariableMap.t) :
    float * literal VariableMap.t =
  Cli.debug_print "beginning compute_deposit@.";
  let update_inputs = update_inputs_var p.ip_program in
  let inputs = inputs |> update_inputs "FLAG_ACO" 1. |> update_inputs "V_CALCUL_ACO" 1. in
  let p = reset_and_add_outputs p [ "MTAP"; "PREM8_11" ] in
  let ctx = Interpreter.evaluate_program p inputs false in
  let inputs = inputs |> update_inputs "V_CALCUL_ACO" 0. |> update_inputs "FLAG_ACO" 2. in
  let acompte = extract_value 0. (get_ctx_var p.ip_program ctx "MTAP") in
  let prem = extract_value 0. (get_ctx_var p.ip_program ctx "PREM8_11") in
  Cli.debug_print "ending compute_deposit@.";
  (acompte, update_inputs "PREM8_11" prem inputs)

(** Equivalent to AC_CalculeAcomptesAvFisc *)
let compute_deposit_with_benefit (p : Interpreter.interpretable_program)
    (inputs : literal VariableMap.t) (napsanpenareel : float) : float * literal VariableMap.t =
  Cli.debug_print "beginning compute_deposit_with_benefit@.";
  let update_inputs = update_inputs_var p.ip_program in
  let inputs = update_inputs "FLAG_ACO" 1. inputs in
  let montantavtmp, inputs = compute_benefit p inputs in
  Cli.debug_print "NAPTEO: %f@." montantavtmp;
  let inputs =
    inputs |> update_inputs "V_INDTEO" 0.
    |> update_inputs "V_NEGREEL" (if napsanpenareel > 0. then 0. else 1.)
    |> update_inputs "V_NAPREEL" (fabs napsanpenareel)
    |> update_inputs "V_CALCUL_ACO" 1.0
  in
  let p = reset_and_add_outputs p [ "MTAP"; "MTAPPS"; "PREM8_11" ] in
  let ctx = Interpreter.evaluate_program p inputs false in
  let inputs = inputs |> update_inputs "V_CALCUL_ACO" 0. |> update_inputs "FLAG_ACO" 2. in
  let acompte = extract_value 0. (get_ctx_var p.ip_program ctx "MTAP") in
  let acompteps = extract_value 0. (get_ctx_var p.ip_program ctx "MTAPPS") in
  let prem = extract_value 0. (get_ctx_var p.ip_program ctx "PREM8_11") in
  let inputs = inputs |> update_inputs "V_ACO_MTAPPS" acompteps |> update_inputs "PREM8_11" prem in
  Cli.debug_print "ending compute_deposit_with_benefit@.";
  (acompte, inputs)

(** Equivalent to IN_Article1731bis in DGFIP's codebase *)
let compute_article1731bis (p : Interpreter.interpretable_program) (inputs : literal VariableMap.t)
    : literal VariableMap.t =
  let update_inputs = update_inputs_var p.ip_program in
  let inputs = update_inputs "ART1731BIS" 0. inputs in
  match get_input_var p.ip_program inputs "CMAJ" with
  | SimpleVar Undefined -> inputs
  | SimpleVar (Float f) ->
      if f == 8. || f == 11. then
        inputs |> update_inputs "ART1731BIS" 1. |> update_inputs "PREM8_11" 1.
      else inputs
  | _ -> assert false

(** Equivalent to IN_traite_double_liquidation3 in DGFiP's codebase *)
let compute_double_liquidation3 (p : Interpreter.interpretable_program)
    (inputs : literal VariableMap.t) (outputs : string list option) :
    Interpreter.ctx * literal VariableMap.t =
  let update_inputs = update_inputs_var p.ip_program in
  let inputs =
    inputs |> update_inputs "FLAG_ACO" 0. |> update_inputs "V_NEGACO" 0.
    |> update_inputs "V_AVFISCOPBIS" 0. |> update_inputs "V_DIFTEOREEL" 0.
    |> update_inputs "PREM8_11" 0.
  in
  let inputs = compute_article1731bis p inputs in
  (* do we need to perform "acomptes" computation? *)
  let calcul_acomptes = exists_deposit_defined_variables inputs in
  (* do we need to perform "avantages fiscal" computation? *)
  let calcul_avfisc = exists_taxbenefit_defined_variables inputs in
  let inputs, v_8ZG =
    if calcul_avfisc then
      match get_inputs_var p.ip_program "8ZG" inputs with
      (* FIXME: check that NULL is indéfini the get_var_irdate. Check TYPE_REVENU too *)
      | Mvg.Float f -> (clear_inputs_var p.ip_program inputs "8ZG", f)
      | _ -> (inputs, 0.)
    else (inputs, 0.)
  in
  Cli.debug_print "Début du calcul des acomptes";
  let inputs, acompte, indice_aco =
    if calcul_acomptes && false (* according to the call?*)
                                (* and p_IsCalculAcomptes? *) then
      let inputs, inputs_other = partition_inputs var_is_deposit inputs in
      let acompte, inputs =
        if calcul_avfisc then compute_deposit_with_benefit p inputs 0.
          (* FIXME: optimize since last param seems to always be 0 *)
        else compute_deposit p inputs
      in
      let indice_aco = if acompte >= 0. then 0. else 1. in
      (merge_inputs inputs inputs_other, fabs acompte, indice_aco)
    else (inputs, -1., 1.)
  in
  Cli.debug_print "PREM8_11 = %f"
    (extract_value (-1.) (get_input_var p.ip_program inputs "PREM8_11"));

  Cli.debug_print "Fin du calcul des acomptes@.Début du calcul de plafonnement@.";
  let inputs =
    if calcul_avfisc then
      let inputs =
        inputs |> update_inputs "V_AVFISCOPBIS" 0. |> update_inputs "V_DIFTEOREEL" 0.
        |> update_inputs "V_INDTEO" 1.
      in
      let _, inputs = compute_benefit p inputs in
      inputs |> update_inputs "V_INDTEO" 0. |> update_inputs "V_NEGREEL" 1.
      |> update_inputs "V_NAPREEL" 0.
      (* FIXME: normalement c'est l_montantreel mais je pense qu'après 2018 il n'est plus assigné *)
    else inputs
  in
  let inputs = update_inputs "8ZG" v_8ZG inputs in
  Cli.debug_print "Valorisation de l'acompte %f, V_INDTEO=%f" acompte
    (match get_inputs_var p.ip_program "V_INDTEO" inputs with Float f -> f | Undefined -> 0.);
  let inputs =
    (* to check: conditional unecessary due to default values? *)
    if acompte > 0. then
      inputs |> update_inputs "V_ACO_MTAP" acompte |> update_inputs "V_NEGACO" indice_aco
    else inputs |> update_inputs "V_ACO_MTAP" 0. |> update_inputs "V_NEGACO" 0.
  in
  let p = match outputs with None -> p | Some outputs -> reset_and_add_outputs p outputs in
  let ctx = Interpreter.evaluate_program p inputs false in
  (ctx, inputs)

(** Equivalent to IN_traite_double_exit_taxe in DGFiP's codebase *)
let compute_double_liquidation_exit_taxe (p : Interpreter.interpretable_program)
    (inputs : literal VariableMap.t) (outputs : string list option) :
    Interpreter.ctx * literal VariableMap.t =
  let update_inputs = update_inputs_var p.ip_program in
  let montant3WA = get_input_var p.ip_program inputs "PVSURSI" in
  let montant3WB = get_input_var p.ip_program inputs "PVIMPOS" in
  let montantRWB = get_input_var p.ip_program inputs "CODRWB" in
  let inputs, p =
    match (montant3WB, montantRWB) with
    | SimpleVar Undefined, SimpleVar Undefined -> (inputs, p)
    | _ ->
        let inputs = inputs |> update_inputs "FLAG_EXIT" 1. |> update_inputs "FLAG_3WBNEG" 0. in
        let ctx, inputs =
          compute_double_liquidation3 p inputs
            (Some [ "NAPTIR"; "IHAUTREVT"; "ID11"; "RASTXFOYER" ])
        in
        let inputs =
          match get_ctx_var p.ip_program ctx "NAPTIR" with
          | SimpleVar (Float l_Montant) ->
              let inputs =
                if l_Montant < 0. then update_inputs "FLAG_3WBNEG" 1. inputs (* FIXME: NAPTIR? *)
                else inputs
              in
              update_inputs "V_NAPTIR3WB" (fabs l_Montant) inputs
          | SimpleVar Undefined -> inputs
          | _ -> assert false
        in
        let inputs =
          if !Cli.year >= 2017 then
            match get_ctx_var p.ip_program ctx "IHAUTREVT" with
            | SimpleVar (Float f) -> update_inputs "V_CHR3WB" f inputs
            | _ -> inputs
          else inputs
        in
        let inputs =
          if !Cli.year >= 2018 then
            match get_ctx_var p.ip_program ctx "ID11" with
            | SimpleVar (Float f) -> update_inputs "V_ID113WB" f inputs
            | _ -> inputs
          else inputs
        in
        (update_inputs "FLAG_EXIT" 0. inputs, p)
  in
  let montantRWA = get_input_var p.ip_program inputs "CODRWA" in
  let inputs, p =
    match (montant3WA, montantRWA) with
    | SimpleVar Undefined, SimpleVar Undefined -> (inputs, p)
    | _ ->
        let inputs = inputs |> update_inputs "FLAG_3WANEG" 0. |> update_inputs "FLAG_EXIT" 2. in
        let ctx, inputs =
          compute_double_liquidation3 p inputs (Some [ "NAPTIR"; "IHAUTREVT"; "ID11" ])
        in
        let inputs =
          match get_ctx_var p.ip_program ctx "NAPTIR" with
          | SimpleVar (Float l_Montant) ->
              let inputs =
                if l_Montant < 0. then (* FIXME: NAPTIR? *)
                  update_inputs "FLAG_3WANEG" 1. inputs
                else inputs
              in
              update_inputs "V_NAPTIR3WA" (fabs l_Montant) inputs
          | SimpleVar Undefined -> inputs
          | _ -> assert false
        in
        let inputs =
          if !Cli.year >= 2017 then
            match get_ctx_var p.ip_program ctx "IHAUTREVT" with
            | SimpleVar (Float f) -> update_inputs "V_CHR3WA" f inputs
            | _ -> inputs
          else inputs
        in
        let inputs =
          if !Cli.year >= 2018 then
            match get_ctx_var p.ip_program ctx "ID11" with
            | SimpleVar (Float f) -> update_inputs "V_ID113WA" f inputs
            | _ -> inputs
          else inputs
        in
        (update_inputs "FLAG_EXIT" 0. inputs, p)
  in
  let inputs, p =
    if !Cli.year >= 2018 then
      let inputs = update_inputs "FLAG_BAREM" 1.0 inputs in
      let ctx, inputs =
        compute_double_liquidation3 p inputs
          (Some [ "RASTXFOYER"; "RASTXDEC1"; "RASTXDEC2"; "INDTAZ"; "IITAZIR"; "IRTOTAL" ])
      in
      let inputs =
        match get_ctx_var p.ip_program ctx "RASTXFOYER" with
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation exit taxe, RASTXFOYER = %f@." f;
            update_inputs "V_BARTXFOYER" f inputs
        | _ -> inputs
      in
      let inputs =
        match get_ctx_var p.ip_program ctx "RASTXDEC1" with
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation exit taxe, RASTXDEC1 = %f@." f;
            update_inputs "V_BARTXDEC1" f inputs
        | _ -> inputs
      in
      let inputs =
        match get_ctx_var p.ip_program ctx "RASTXDEC2" with
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation exit taxe, RASTXDEC2 = %f@." f;
            update_inputs "V_BARTXDEC2" f inputs
        | _ -> inputs
      in
      let inputs =
        match get_ctx_var p.ip_program ctx "INDTAZ" with
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation exit taxe, INDTAZ = %f@." f;
            update_inputs "V_BARINDTAZ" f inputs
        | _ -> inputs
      in
      let inputs =
        match get_ctx_var p.ip_program ctx "IITAZIR" with
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation exit taxe, IITAZIR = %f@." f;
            let flag, f = if f < 0. then (1., -.f) else (0., f) in
            inputs |> update_inputs "FLAG_BARIITANEG" flag |> update_inputs "V_BARIITAZIR" f
        | _ -> inputs
      in
      let inputs =
        match get_ctx_var p.ip_program ctx "IRTOTAL" with
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation exit taxe, IRTOTAL = %f@." f;
            update_inputs "V_BARIRTOTAL" f inputs
        | _ -> inputs
      in
      (update_inputs "FLAG_BAREM" 0. inputs, p)
    else (inputs, p)
  in
  compute_double_liquidation3 p inputs outputs

(** Equivalent to IN_traite_double_liquidation_pvro in DGFiP's codebase *)
let compute_double_liquidation_pvro (p : Interpreter.interpretable_program)
    (inputs : literal VariableMap.t) : Interpreter.ctx * literal VariableMap.t =
  let update_inputs = update_inputs_var p.ip_program in
  let inputs =
    match get_input_var p.ip_program inputs "COD3WG" with
    | SimpleVar Undefined -> inputs
    | _ -> (
        let inputs = update_inputs "FLAG_PVRO" 1. inputs in
        let ctx, inputs = compute_double_liquidation_exit_taxe p inputs (Some [ "IAD11" ]) in
        match get_ctx_var p.ip_program ctx "IAD11" with
        | SimpleVar Undefined -> inputs
        | SimpleVar (Float f) ->
            Cli.debug_print "double liquidation pvro : IAD11 = %f" f;
            update_inputs "V_IPVRO" f inputs
        | _ -> assert false )
  in
  let inputs = update_inputs "FLAG_PVRO" 0. inputs in
  compute_double_liquidation_exit_taxe p inputs None

let compute_program (p : Interpreter.interpretable_program) (inputs : literal VariableMap.t) :
    Interpreter.ctx =
  (* in primitive mode, V_IND_TRAIT is set to 4 (5 in corrective mode) *)
  let inputs = update_inputs_var p.ip_program "V_IND_TRAIT" 4. inputs in
  let _, inputs = compute_double_liquidation_pvro p inputs in
  (* in a last pass, we also check the verification conditions *)
  let ctx = Interpreter.evaluate_program p inputs true in
  ctx
