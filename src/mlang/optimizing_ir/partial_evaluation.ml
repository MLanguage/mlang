(* Copyright (C) 2020 Inria, contributors: Denis Merigoux <denis.merigoux@inria.fr>

   This program is free software: you can redistribute it and/or modify it under the terms of the
   GNU General Public License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
   even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License along with this program. If
   not, see <https://www.gnu.org/licenses/>. *)

(* FIXME: new definedness optimizations *)
open Oir

type partial_expr = PartialLiteral of Mir.literal | UnknownFloat

type definedness = Bot | Undefined | Float | Top
let format_definedness fmt d =
  Format.fprintf fmt (match d with
                      | Bot -> "bot"
                      | Undefined -> "undef"
                      | Float -> "float"
                      | Top -> "top")

let maybe_undefined = function Undefined | Top -> true | _ -> false

let maybe_float = function Float | Top -> true | _ -> false

let join d1 d2 =
  match (d1, d2) with
  | Bot, _ -> d2
  | _, Bot -> d1
  | Top, _ | _, Top -> Top
  | Undefined, Float | Float, Undefined -> Top
  | Undefined, Undefined -> Undefined
  | Float, Float -> Float

(* operations where undef is absorbing, such as the multiplication *)
let undef_absorb d1 d2 =
  if d1 = Bot || d2 = Bot then Bot
  else if maybe_float d1 && maybe_float d2 then
    if maybe_undefined d1 || maybe_undefined d2 then Top else Float
  else if maybe_undefined d1 || maybe_undefined d2 then Undefined
  else assert false

(* operations where a single undef is cast to 0, such as the addition *)
let undef_cast d1 d2 =
  if maybe_undefined d1 && maybe_undefined d2 then
    if maybe_float d1 || maybe_float d2 then Top else Undefined
  else if maybe_float d1 || maybe_float d2 then Float
  else assert false

let from_literal l = (Mir.Literal l, match l with Float _ -> Float | Undefined -> Undefined)

let partial_to_expr (e : partial_expr) : Mir.expression option * definedness =
  match e with
  | PartialLiteral l ->
      let e, d = from_literal l in
      (Some e, d)
  | UnknownFloat -> (None, Float)

let expr_to_partial (e : Mir.expression) (d : definedness) : partial_expr option =
  match (e, d) with
  | Literal l, _ -> Some (PartialLiteral l)
  | _, Undefined -> Some (PartialLiteral Undefined)
  | _, Float -> Some UnknownFloat
  | _ -> None

type var_literal = SimpleVar of partial_expr | TableVar of int * partial_expr array

type partial_ev_ctx = {
  ctx_local_vars : partial_expr Mir.LocalVariableMap.t;
  ctx_vars : var_literal option BlockMap.t Mir.VariableMap.t;
      (** The option at the leaves of the [BlockMap.t] is to account for definitions that are not
          literals but that are to be taken into account for validity of inlining later *)
  ctx_doms : Dominators.dom;
  ctx_paths : Paths.path_checker;
  ctx_entry_block : block_id;
  ctx_inside_block : block_id option;
}

let empty_ctx (g : CFG.t) (entry_block : block_id) =
  {
    ctx_local_vars = Mir.LocalVariableMap.empty;
    ctx_vars = Mir.VariableMap.empty;
    ctx_doms = Dominators.idom_to_dom (Dominators.compute_idom g entry_block);
    ctx_paths = Paths.create g;
    ctx_inside_block = None;
    ctx_entry_block = entry_block;
  }

let reset_ctx (ctx : partial_ev_ctx) (block_id : block_id) =
  { ctx with ctx_local_vars = Mir.LocalVariableMap.empty; ctx_inside_block = Some block_id }

let peval_debug = ref false

let compare_for_min_dom (dom : Dominators.dom) (id1 : block_id) (id2 : block_id) : int =
  (* the sort puts smaller items first, and we want the first item to be the block that is less
     dominating. So if id1 dominates id2, we want id2 to be smaller hence returning a positive value *)
  if dom id1 id2 then 1 else if dom id2 id1 then -1 else 0

let add_var_def_to_ctx (ctx : partial_ev_ctx) (block_id : block_id) (var : Mir.Variable.t)
    (var_lit : var_literal option) : partial_ev_ctx =
  {
    ctx with
    ctx_vars =
      Mir.VariableMap.add var
        ( match Mir.VariableMap.find_opt var ctx.ctx_vars with
        | None -> BlockMap.singleton block_id var_lit
        | Some defs -> BlockMap.add block_id var_lit defs )
        ctx.ctx_vars;
  }

let get_closest_dominating_def (var : Mir.Variable.t) (ctx : partial_ev_ctx) :
    (block_id * var_literal) option =
  let curr_block = Option.get ctx.ctx_inside_block in
  match Mir.VariableMap.find_opt var ctx.ctx_vars with
  | None ->
     None
  | Some defs -> (
      let previous_defs = Mir.VariableMap.find var ctx.ctx_vars in
      let dominating_defs =
        BlockMap.bindings
          (BlockMap.filter
             (fun def_block_id def ->
               Option.is_some def
               (* we only consider definitions that were partially evaluated to a literal *)
               && ( def_block_id = Option.get ctx.ctx_inside_block
                  || ctx.ctx_doms def_block_id curr_block ))
             previous_defs)
      in
      let sorted_dominating_defs =
        List.sort (fun (b1, _) (b2, _) -> compare_for_min_dom ctx.ctx_doms b1 b2) dominating_defs
      in
      match sorted_dominating_defs with
      | [] ->
         None
      | (_, None) :: _ -> assert false (* should not happen *)
      | (def_block, Some def) :: _ ->
          (* Now we have the closest def in a block that dominates the current block. But something
             could go wrong in the case where a branch of an if in between the current block and the
             def block redefines the variable. So we have to check that they are no such defs *)
          if def_block = Option.get ctx.ctx_inside_block then Some (def_block, def)
          else
            let exists_other_def_in_between =
              BlockMap.exists
                (fun intermediate_block _ ->
                  if intermediate_block = def_block || intermediate_block = curr_block then false
                  else
                    Paths.check_path ctx.ctx_paths def_block intermediate_block
                    && Paths.check_path ctx.ctx_paths intermediate_block curr_block)
                defs
            in
            if exists_other_def_in_between then None
            else Some (def_block, def) )

let interpreter_ctx_from_partial_ev_ctx (ctx : partial_ev_ctx) : Bir_interpreter.ctx =
  {
    Bir_interpreter.empty_ctx with
    Bir_interpreter.ctx_vars =
      Mir.VariableMap.map Option.get
        (Mir.VariableMap.filter
           (fun _ x -> Option.is_some x)
           (Mir.VariableMap.mapi
              (fun var _ ->
                match get_closest_dominating_def var ctx with
                | Some (_, SimpleVar (PartialLiteral l)) -> Some (Bir_interpreter.SimpleVar l)
                | _ -> None)
              ctx.ctx_vars));
  }

let (=>) b1 b2 = not b1 || b2
let check e d =
  if d = Undefined && Pos.unmark e <> Mir.Literal Undefined then
    failwith "check"
  else match Pos.unmark e with
       | Literal Undefined when d <> Undefined -> failwith "check"
       | Literal (Float _) when d <> Float -> failwith "check"
       | _ -> ()


let rec partially_evaluate_expr (ctx : partial_ev_ctx) (p : Mir.program)
    (e : Mir.expression Pos.marked) : Mir.expression Pos.marked * definedness =
  let new_e, d = match Pos.unmark e with
  | Comparison (op, e1, e2) ->
      let new_e1, d1 = partially_evaluate_expr ctx p e1 in
      let new_e2, d2 = partially_evaluate_expr ctx p e2 in
      ( Pos.same_pos_as
          begin
            match (Pos.unmark new_e1, Pos.unmark new_e2) with
            | Literal Undefined, _ | _, Literal Undefined -> Mir.Literal Undefined
            | Literal _, Literal _ ->
                Mir.Literal
                  (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                     (Pos.same_pos_as (Mir.Comparison (op, new_e1, new_e2)) e))
            | _ ->
               if d1 = Undefined || d2 = Undefined then Mir.Literal Undefined
               else
                 Comparison (op, new_e1, new_e2)
          end
          e,
        undef_absorb d1 d2 )
  | Binop (op, e1, e2) ->
      let new_e1, d1 = partially_evaluate_expr ctx p e1 in
      let new_e2, d2 = partially_evaluate_expr ctx p e2 in
      let new_e, d =
        match (Pos.unmark op, Pos.unmark new_e1, Pos.unmark new_e2) with
        (* calling the interpreter when verything is a literal *)
        | _, Literal _, Literal _ ->
            from_literal
              (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                 (Pos.same_pos_as (Mir.Binop (op, new_e1, new_e2)) e1))
            (* first all the combinations giving undefined *)
        | Mast.And, Literal Undefined, _ -> from_literal Undefined
        | Mast.And, _, Literal Undefined -> from_literal Undefined
        | Mast.Or, Literal Undefined, _ -> from_literal Undefined
        | Mast.Or, _, Literal Undefined -> from_literal Undefined
        | Mast.Mul, _, Literal Undefined -> from_literal Undefined
        | Mast.Mul, Literal Undefined, _ -> from_literal Undefined
        | Mast.Div, Literal Undefined, _ -> from_literal Undefined
        | Mast.Div, _, Literal Undefined -> from_literal Undefined
        (* logical or *)
        | Mast.Or, Literal (Float f), _ when f <> 0. -> from_literal Mir.true_literal
        | Mast.Or, _, Literal (Float f) when f <> 0. -> from_literal Mir.true_literal
        | Mast.Or, Literal (Float 0.), _ -> (Pos.unmark new_e2, d2)
        | Mast.Or, _, Literal (Float 0.) -> (Pos.unmark new_e1, d1)
        (* logican and *)
        | Mast.And, Literal (Float 0.), _ -> from_literal Mir.false_literal
        | Mast.And, _, Literal (Float 0.) -> from_literal Mir.false_literal
        | Mast.And, Literal (Float f), _ when f <> 0. -> (Pos.unmark new_e2, d2)
        | Mast.And, _, Literal (Float f) when f <> 0. -> (Pos.unmark new_e1, d1)
        (* addition *)
        | Mast.Add, Literal Undefined, _ -> (Pos.unmark new_e2, d2)
        | Mast.Add, _, Literal Undefined -> (Pos.unmark new_e1, d1)
        | Mast.Add, _, Literal (Float f) when f < 0. ->
            ( Binop (Pos.same_pos_as Mast.Sub op, e1, Pos.same_pos_as (Mir.Literal (Float (-.f))) e2),
              undef_cast d1 Float )
        | Mast.Add, _, Unop (Minus, e2') ->
           (Binop (Pos.same_pos_as Mast.Sub op, new_e1, e2'), undef_cast d1 d2)
        | (Mast.Add | Mast.Sub), Literal (Float 0.), _ when d2 = Float ->
           Pos.unmark new_e2, d2
        | (Mast.Add | Mast.Sub), _, Literal (Float 0.) when d1 = Float ->
           Pos.unmark new_e1, d1
        (* substraction *)
        | Mast.Sub, e1, e2 when e1 = e2 && e1 <> Literal Undefined && e2 <> Literal Undefined ->
            from_literal (Float 0.)
        | Mast.Sub, Literal Undefined, e' ->
            (Unop (Minus, Pos.same_pos_as e' e), undef_cast Undefined d2)
        | Mast.Sub, e', Literal Undefined -> (e', undef_cast d1 Undefined)
        (* multiplication *)
        | Mast.Mul, Literal (Float 1.), e' -> (e', undef_cast Float d2)
        | Mast.Mul, e', Literal (Float 1.) -> (e', undef_cast d1 Float)
        (* we can't optimize float multiplication by 0 here in the general case, because Undefined.
           But if we know that the other term can't be undefined, then we can go ! This is the case
           for the result of some functions *)
        | Mast.Mul, Literal (Float 0.), _ when d2 = Float -> from_literal (Float 0.)
        | Mast.Mul, _, Literal (Float 0.) when d1 = Float -> from_literal (Float 0.)
        (* division *)
        | Mast.Div, e', Literal (Float 1.) -> (e', d1)
        | Mast.Div, Literal (Float 0.), _ -> from_literal (Mir.Float 0.)
        (* default case *)
        | (Mast.Add | Mast.Sub), _, _ -> (Binop (op, new_e1, new_e2), undef_cast d1 d2)
        | (Mast.Mul | Mast.Div | Mast.And | Mast.Or), _, _ ->
            (Binop (op, new_e1, new_e2), undef_absorb d1 d2)
      in
      (Pos.same_pos_as new_e e, d)
  | Unop (op, e1) ->
      let new_e1, d1 = partially_evaluate_expr ctx p e1 in
      let new_e, d =
        match Pos.unmark new_e1 with
        | Literal _ ->
            from_literal
              (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p
                 (Pos.same_pos_as (Mir.Unop (op, new_e1)) e1))
        | _ -> (
            ( Unop (op, new_e1),
              match (op, d1) with
              | Not, Float -> Float
              | Not, Undefined -> Undefined
              | Not, Top -> Top
              | Minus, Float -> Float
              | Minus, Undefined -> Float
              | Minus, Top -> Float
              | _ -> assert false ) )
      in
      (Pos.same_pos_as new_e e, d)
  | Conditional (e1, e2, e3) -> (
      let new_e1, d1 = partially_evaluate_expr ctx p e1 in
      let new_e2, d2 = partially_evaluate_expr ctx p e2 in
      let new_e3, d3 = partially_evaluate_expr ctx p e3 in
      match Pos.unmark new_e1 with
      | Literal (Float 0.) -> (new_e3, d3)
      | Literal (Float _) -> (new_e2, d2)
      | _ ->
          if Pos.unmark new_e1 = Literal Undefined || d1 = Undefined then
            (Pos.same_pos_as (Mir.Literal Undefined) e, Undefined)
          else (Pos.same_pos_as (Mir.Conditional (new_e1, new_e2, new_e3)) e, join d2 d3) )
  | Index (var, e1) ->
      let new_e1, d1 = partially_evaluate_expr ctx p e1 in
      let new_e, d =
        match Pos.unmark new_e1 with
        | Literal Undefined -> from_literal Undefined
        | Literal (Float f) -> (
            let idx =
              if
                let fraction, _ = modf f in
                fraction = 0.
              then int_of_float f
              else
                let err, ctx =
                  ( Bir_interpreter.FloatIndex
                      (Format.asprintf "%a" Pos.format_position (Pos.get_position e1)),
                    interpreter_ctx_from_partial_ev_ctx ctx )
                in
                if !Bir_interpreter.exit_on_rte then
                  Bir_interpreter.raise_runtime_as_structured err ctx p
                else raise (Bir_interpreter.RuntimeError (err, ctx))
            in
            match get_closest_dominating_def (Pos.unmark var) ctx with
            | Some (_, SimpleVar _) -> assert false (* should not happen *)
            | Some (_, TableVar (size, es')) -> (
                if idx >= size || idx < 0 then from_literal Undefined
                else
                  match es'.(idx) with
                  | PartialLiteral e' -> from_literal e'
                  | UnknownFloat -> (Mir.Index (var, new_e1), Float) )
            | None -> (Mir.Index (var, new_e1), Top) )
        | _ -> if d1 = Undefined then from_literal Undefined else (Mir.Index (var, new_e1), Top)
      in
      (Pos.same_pos_as new_e e, d)
  | Literal l -> ( (e, match l with Undefined -> Undefined | Float _ -> Float) )
  | Var var -> (
      match get_closest_dominating_def var ctx with
      | Some (_, SimpleVar pexpr ) ->
         let oe, d = partial_to_expr pexpr in
         let new_e = match oe with | Some e' -> e' | None -> Pos.unmark e in
         (Pos.same_pos_as new_e e, d)
      | Some (_, TableVar _) ->
          (e, Top) (* this case happens when calling functions like "multimax" *)
            (* FIXME: definedness? *)
      | _ ->
         (e, Top) )
  | LocalVar lvar -> (
      try
        (* FIXME *)
        let oe, d = partial_to_expr (Mir.LocalVariableMap.find lvar ctx.ctx_local_vars) in
        match oe with None -> (e, d) | Some e' -> (Pos.same_pos_as e' e, d)
      with Not_found -> (e, Top) )
  | GenericTableIndex -> (e, Float)
  | Error -> (e, Top) (* FIXME *)
  (* let l1 = b in l2 *)
  | LocalLet (l1, b, (LocalVar l2, _)) when Mir.LocalVariable.compare l1 l2 = 0 ->
      partially_evaluate_expr ctx p b
  | LocalLet (lvar, e1, e2) -> (
      let new_e1, d1 = partially_evaluate_expr ctx p e1 in
      match Pos.unmark new_e1 with
      | Literal _ ->
          let new_ctx =
            {
              ctx with
              ctx_local_vars =
                Mir.LocalVariableMap.add lvar
                  (Option.get (expr_to_partial (Pos.unmark new_e1) d1))
                  ctx.ctx_local_vars;
            }
          in
          partially_evaluate_expr new_ctx p e2
      | _ ->
          if d1 = Undefined then
            let new_ctx =
              {
                ctx with
                ctx_local_vars =
                  Mir.LocalVariableMap.add lvar
                    (Option.get (expr_to_partial (Literal Undefined) d1))
                    ctx.ctx_local_vars;
              }
            in
            partially_evaluate_expr new_ctx p e2
          else
            let new_e2, d2 = partially_evaluate_expr ctx p e2 in
            let new_e, d =
              match Pos.unmark new_e2 with
              | Literal l -> from_literal l
              | _ -> (Mir.LocalLet (lvar, new_e1, new_e2), d2)
            in
            (Pos.same_pos_as new_e e, d) )
  | FunctionCall (func, args) ->
      let new_args, new_ds =
        List.split @@ List.map (fun arg -> partially_evaluate_expr ctx p arg) args
      in
      let all_args_literal =
        List.for_all
          (fun arg -> match Pos.unmark arg with Mir.Literal _ -> true | _ -> false)
          new_args
      in
      let new_e = Pos.same_pos_as (Mir.FunctionCall (func, new_args)) e in
      let new_e, d =
        if all_args_literal then
          from_literal (Bir_interpreter.evaluate_expr Bir_interpreter.empty_ctx p new_e)
        else
          match func with
          | ArrFunc | InfFunc -> (Pos.unmark new_e, List.hd new_ds)
          | PresentFunc -> (Pos.unmark new_e, Float)
          | MinFunc | MaxFunc | Multimax -> (Pos.unmark new_e, Float)
          | _ -> assert false
      in
      (Pos.same_pos_as new_e e, d)
  in
  check new_e d;
  if !peval_debug then Cli.debug_print "%a ~> %a, d = %a" Format_mir.format_expression (Pos.unmark e) Format_mir.format_expression (Pos.unmark new_e) format_definedness d;
  new_e, d

let debug_vars = []

let partially_evaluate_stmt (stmt : stmt) (block_id : block_id) (ctx : partial_ev_ctx)
    (new_block : stmt list) (p : program) : stmt list * partial_ev_ctx =
  let ctx = reset_ctx ctx block_id in
  match Pos.unmark stmt with
  | SAssign (var, def) ->
      if List.mem (Pos.unmark var.name) debug_vars then peval_debug := true;
      let new_def, new_ctx =
        match def.var_definition with
        | InputVar -> (Mir.InputVar, ctx)
        | SimpleVar e ->
            let e', d' = partially_evaluate_expr ctx p.mir_program e in
            let partial_e' =
              Option.map (fun x -> SimpleVar x) (expr_to_partial (Pos.unmark e') d')
            in
            (SimpleVar e', add_var_def_to_ctx ctx block_id var partial_e')
        | TableVar (size, def) -> (
            match def with
            | IndexGeneric e ->
                let e', d' = partially_evaluate_expr ctx p.mir_program e in
                let partial_e' =
                  Option.map
                    (fun x -> TableVar (size, Array.init size (fun _ -> x)))
                    (expr_to_partial (Pos.unmark e') d')
                in
                (TableVar (size, IndexGeneric e'), add_var_def_to_ctx ctx block_id var partial_e')
            | IndexTable es ->
                let es' =
                  Mir.IndexMap.mapi (fun _ e -> partially_evaluate_expr ctx p.mir_program e) es
                in
                let new_ctx =
                  if
                    Mir.IndexMap.for_all
                      (fun _ (e, d) -> Option.is_some (expr_to_partial (Pos.unmark e) d))
                      es'
                  then
                    add_var_def_to_ctx ctx block_id var
                      (Some
                         (TableVar
                            ( size,
                              Array.init size (fun i ->
                                  let e, d = Mir.IndexMap.find i es' in
                                  Option.get (expr_to_partial (Pos.unmark e) d)) )))
                  else add_var_def_to_ctx ctx block_id var None
                in
                (TableVar (size, IndexTable (Mir.IndexMap.map fst es')), new_ctx) )
      in
      if List.mem (Pos.unmark var.name) debug_vars then peval_debug := false;
      let new_stmt = Pos.same_pos_as (SAssign (var, { def with var_definition = new_def })) stmt in
      (new_stmt :: new_block, new_ctx)
  | SConditional (e, b1, b2, join) -> (
      let new_e, d = partially_evaluate_expr ctx p.mir_program (e, Pos.no_pos) in
      match expr_to_partial (Pos.unmark new_e) d with
      | Some (PartialLiteral (Float 0.0)) -> (Pos.same_pos_as (SGoto b2) stmt :: new_block, ctx)
      | Some (PartialLiteral (Float _)) -> (Pos.same_pos_as (SGoto b1) stmt :: new_block, ctx)
      | Some (PartialLiteral Undefined) -> (Pos.same_pos_as (SGoto join) stmt :: new_block, ctx)
      | _ -> (Pos.same_pos_as (SConditional (Pos.unmark new_e, b1, b2, join)) stmt :: new_block, ctx)
      )
  | SVerif cond -> (
      let new_e, d = partially_evaluate_expr ctx p.mir_program cond.cond_expr in
      match expr_to_partial (Pos.unmark new_e) d with
      | Some (PartialLiteral (Undefined | Float 0.0)) -> (new_block, ctx)
      | Some (PartialLiteral (Float _)) ->
          Cli.error_print "Error during partial evaluation!";
          let err, ctx =
            ( Bir_interpreter.ConditionViolated (cond.cond_errors, cond.cond_expr, []),
              interpreter_ctx_from_partial_ev_ctx ctx )
          in
          if !Bir_interpreter.exit_on_rte then
            Bir_interpreter.raise_runtime_as_structured err ctx p.mir_program
          else raise (Bir_interpreter.RuntimeError (err, ctx))
      | _ -> (Pos.same_pos_as (SVerif { cond with cond_expr = new_e }) stmt :: new_block, ctx) )
  | _ -> (stmt :: new_block, ctx)

let partial_evaluation (p : program) : program =
  let g = get_cfg p in
  let p, _ =
    Topological.fold
      (fun (block_id : block_id) (p, ctx) ->
        let block = BlockMap.find block_id p.blocks in
        let new_block, ctx =
          List.fold_left
            (fun (new_block, ctx) stmt -> partially_evaluate_stmt stmt block_id ctx new_block p)
            ([], ctx) block
        in
        ({ p with blocks = BlockMap.add block_id (List.rev new_block) p.blocks }, ctx))
      g
      (p, empty_ctx g p.entry_block)
  in
  p
