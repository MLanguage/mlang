type offset =
  | GetValueConst of int
  | GetValueVar of Bir.variable
  | PassPointer
  | None

let rec generate_variable (vm : Dgfip_varid.var_id_map) (offset : offset)
    ?(def_flag = false) ?(debug_flag = false) (var : Bir.variable) : string =
  let mvar = Bir.var_to_mir var in
  try
    match offset with
    | PassPointer ->
        if def_flag then Dgfip_varid.gen_access_def_pointer vm mvar
        else Dgfip_varid.gen_access_pointer vm mvar
    | _ ->
        let offset =
          match offset with
          | None -> ""
          | GetValueVar offset -> " + (int)" ^ generate_variable vm None offset
          | GetValueConst offset -> " + " ^ string_of_int offset
          | PassPointer -> assert false
        in
        if def_flag then Dgfip_varid.gen_access_def vm mvar offset
        else
          let access_val = Dgfip_varid.gen_access_val vm mvar offset in
          if debug_flag then
            let vn = Pos.unmark mvar.Mir.Variable.name in
            let pos_tgv = Dgfip_varid.gen_access_pos_from_start vm mvar in
            Format.asprintf "(aff3(\"%s\",irdata, %s), %s)" vn pos_tgv
              access_val
          else access_val
  with Not_found ->
    Errors.raise_error
      (Format.asprintf "Variable %s not found in TGV"
         (Pos.unmark mvar.Mir.Variable.name))

type local_var = Anon | Refered of int

type dflag = Def | Val

type stack_slot = { kind : dflag; depth : int }

type stack_assignment = { slot : stack_slot; subexpr : expr }

and local_stacks = {
  def_top : int;
  val_top : int;
  var_substs : (int * (expr * dflag)) list;
}

and local_vars = (local_var * stack_assignment) list

and expr =
  | Dtrue
  | Dfalse
  | Dlit of float
  | Dvar of expr_var
  | Dand of expr * expr
  | Dor of expr * expr
  | Dunop of string * expr
  | Dbinop of string * expr * expr
  | Dfun of string * expr list
  | Daccess of Bir.variable * dflag * expr
  | Dite of expr * expr * expr

and expr_var = Local of stack_slot | M of Bir.variable * offset * dflag

and t = expr * dflag * local_vars

and constr = local_stacks -> local_vars -> t

type expression_composition = { def_test : constr; value_comp : constr }

type stack_position = Not_to_stack | Must_be_pushed | On_top of dflag

let is_always_true ((expr, _kind, _lv) : t) = expr = Dtrue

let cast (kind : dflag) (expr : expr) =
  match (expr, kind) with
  | Dtrue, Val -> Dlit 1.
  | Dfalse, Val -> Dlit 0.
  | Dlit 0., Def -> Dfalse
  | Dlit _, Def -> Dtrue
  | _, Def -> Dbinop ("!=", expr, Dlit 0.)
  | _, Val -> expr

(** local stacks operations *)

let bump_stack (kind : dflag) (st : local_stacks) =
  match kind with
  | Def -> { st with def_top = st.def_top + 1 }
  | Val -> { st with val_top = st.val_top + 1 }

let add_substitution (st : local_stacks) (v : local_var) (kind : dflag)
    (expr : expr) =
  match v with
  | Anon -> st
  | Refered v -> { st with var_substs = (v, (expr, kind)) :: st.var_substs }

let stack_top (kind : dflag) (st : local_stacks) =
  match kind with Def -> st.def_top | Val -> st.val_top

let is_in_stack_scope ({ kind; depth } : stack_slot) (st : local_stacks) =
  match kind with Def -> depth < st.def_top | Val -> depth < st.val_top

let is_on_top ({ kind; depth } : stack_slot) (st : local_stacks) =
  match kind with Def -> depth = st.def_top | Val -> depth = st.val_top

let expr_position (expr : expr) (st : local_stacks) =
  match expr with
  | Dtrue | Dfalse | Dlit _ | Dvar (M _) -> Not_to_stack
  | Dvar (Local slot) ->
      if is_in_stack_scope slot st then Not_to_stack
      else if is_on_top slot st then On_top slot.kind
      else Must_be_pushed
  | _ -> Must_be_pushed

let store_local (st : local_stacks) (ctx : local_vars) (v : local_var)
    (kind : dflag) (subexpr : expr) =
  match expr_position subexpr st with
  | Not_to_stack -> (add_substitution st v kind subexpr, ctx, subexpr)
  | On_top kind ->
      (add_substitution st v kind subexpr |> bump_stack kind, ctx, subexpr)
  | Must_be_pushed ->
      let depth = stack_top kind st in
      let st = bump_stack kind st in
      let slot = { kind; depth } in
      let assignment = { slot; subexpr } in
      let ctx = (v, assignment) :: ctx in
      (st, ctx, Dvar (Local slot))

let collapse_constr (st : local_stacks) (ctx : local_vars) (constr : constr) =
  let expr, kind, lv = constr st ctx in
  (expr, kind, lv @ ctx)

let push_as (st : local_stacks) (ctx : local_vars) (kind : dflag)
    (constr : constr) =
  let expr, ekind, lv = constr st ctx in
  let expr = if kind = ekind then expr else cast kind expr in
  store_local st lv Anon kind expr

(** smart constructors *)

let locals_from_m (lvar : Mir.local_variable) =
  ( Refered (-(2 * lvar.Mir.LocalVariable.id)),
    Refered (-((2 * lvar.Mir.LocalVariable.id) + 1)) )

let new_local : unit -> local_var =
  let c = ref 0 in
  fun () ->
    let i = !c in
    incr c;
    Refered i

let let_local (v : local_var) (bound : constr) (body : constr)
    (st : local_stacks) (ctx : local_vars) =
  let bound, kind, lv = collapse_constr st ctx bound in
  let st, ctx, _ = store_local st lv v kind bound in
  collapse_constr st ctx body

let dtrue _st _lv : t = (Dtrue, Def, [])

let dfalse _st _lv : t = (Dfalse, Def, [])

let lit (f : float) _st _lv : t = (Dlit f, Val, [])

let m_var (v : Bir.variable) (offset : offset) (df : dflag) _st _lv : t =
  (Dvar (M (v, offset, df)), df, [])

let local_var (lvar : local_var) (st : local_stacks) (ctx : local_vars) : t =
  match lvar with
  | Anon -> Errors.raise_error "Tried to access anonymous local variable"
  | Refered v -> (
      match List.assoc_opt v st.var_substs with
      | Some (e, kind) -> (e, kind, [])
      | None -> (
          match List.assoc_opt lvar ctx with
          | Some { slot; _ } -> (Dvar (Local slot), slot.kind, [])
          | None -> Errors.raise_error "Local variable not found in context"))

let dand (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t
    =
  let st', lv1, e1 = push_as st ctx Def e1 in
  let _, lv2, e2 = push_as st' ctx Def e2 in
  match (e1, e2) with
  | Dtrue, _ -> (e2, Def, lv2)
  | _, Dtrue -> (e1, Def, lv1)
  | Dfalse, _ | _, Dfalse -> (Dfalse, Def, [])
  | _ -> (Dand (e1, e2), Def, lv2 @ lv1)

let dor (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t =
  let st', lv1, e1 = push_as st ctx Def e1 in
  let _, lv2, e2 = push_as st' ctx Def e2 in
  match (e1, e2) with
  | Dtrue, _ | _, Dtrue -> (Dtrue, Def, [])
  | Dfalse, _ -> (e2, Def, lv2)
  | _, Dfalse -> (e1, Def, lv1)
  | _ -> (Dor (e1, e2), Def, lv2 @ lv1)

let dnot (e : constr) (st : local_stacks) (ctx : local_vars) : t =
  let _, lv, e = push_as st ctx Def e in
  match e with
  | Dtrue -> (Dfalse, Def, [])
  | Dfalse -> (Dtrue, Def, [])
  | Dunop ("!", e) -> (e, Def, lv)
  | _ -> (Dunop ("!", e), Def, lv)

let minus (e : constr) (st : local_stacks) (ctx : local_vars) : t =
  let _, lv, e = push_as st ctx Val e in
  match e with
  | Dlit f -> (Dlit (-.f), Val, [])
  | Dunop ("-", e) -> (e, Val, lv)
  | _ -> (Dunop ("-", e), Val, lv)

let plus (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t
    =
  let st', lv1, e1 = push_as st ctx Val e1 in
  let _, lv2, e2 = push_as st' ctx Val e2 in
  match (e1, e2) with
  | Dlit 0., _ -> (e2, Val, lv2)
  | _, Dlit 0. -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 -> (Dlit (f1 +. f2), Val, [])
  | _ -> (Dbinop ("+", e1, e2), Val, lv2 @ lv1)

let sub (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t =
  let st', lv1, e1 = push_as st ctx Val e1 in
  let _, lv2, e2 = push_as st' ctx Val e2 in
  match (e1, e2) with
  | Dlit 0., _ -> (Dunop ("-", e2), Val, lv2)
  | _, Dlit 0. -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 -> (Dlit (f1 -. f2), Val, [])
  | _ -> (Dbinop ("-", e1, e2), Val, lv2 @ lv1)

let mult (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t
    =
  let st', lv1, e1 = push_as st ctx Val e1 in
  let _, lv2, e2 = push_as st' ctx Val e2 in
  match (e1, e2) with
  | Dlit 1., _ -> (e2, Val, lv2)
  | _, Dlit 1. -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 -> (Dlit (f1 *. f2), Val, [])
  | _ -> (Dbinop ("*", e1, e2), Val, lv2 @ lv1)

let div (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) : t =
  let st', lv1, e1 = push_as st ctx Val e1 in
  let _, lv2, e2 = push_as st' ctx Val e2 in
  match (e1, e2) with
  | _, Dlit 1. -> (e1, Val, lv1)
  | Dlit f1, Dlit f2 ->
      let f = if f2 = 0. then 0. else f1 /. f2 in
      (Dlit f, Val, [])
  | _ -> (Dbinop ("/", e1, e2), Val, lv2 @ lv1)

let comp op (e1 : constr) (e2 : constr) (st : local_stacks) (ctx : local_vars) :
    t =
  let st', lv1, e1 = push_as st ctx Val e1 in
  let _, lv2, e2 = push_as st' ctx Val e2 in
  let comp o =
    match (e1, e2) with
    | Dlit f1, Dlit f2 -> if o f1 f2 then Dtrue else Dfalse
    | Dvar v1, Dvar v2 ->
        if String.equal op "==" && v1 = v2 then Dtrue else Dbinop (op, e1, e2)
    | _ -> Dbinop (op, e1, e2)
  in
  let e =
    match op with
    | "==" -> comp ( = )
    | "!=" -> comp ( <> )
    | "<=" -> comp ( <= )
    | "<" -> comp ( < )
    | ">=" -> comp ( >= )
    | ">" -> comp ( > )
    | _ -> assert false
  in
  (e, Def, lv2 @ lv1)

let dfun (f : string) (args : constr list) (st : local_stacks)
    (ctx : local_vars) : t =
  let (_, lv), args =
    List.fold_left_map
      (fun (st, lv) e ->
        let st, lv', e = push_as st ctx Val e in
        ((st, lv' @ lv), e))
      (st, []) args
  in
  (* TODO : distinguish kinds *)
  (Dfun (f, args), Val, lv)

let access (var : Bir.variable) (df : dflag) (e : constr) (st : local_stacks)
    (ctx : local_vars) : t =
  let _, lv, e = push_as st ctx Val e in
  (Daccess (var, df, e), df, lv)

let ite (c : constr) (t : constr) (e : constr) (st : local_stacks)
    (ctx : local_vars) : t =
  let st', lvc, c = push_as st ctx Def c in
  let st', lvt, t = push_as st' ctx Val t in
  let _, lve, e = push_as st' ctx Val e in
  match (c, t, e) with
  | Dtrue, _, _ -> (t, Val, lvt)
  | Dfalse, _, _ -> (e, Val, lve)
  | _, Dlit 1., Dlit 0. -> (c, Val, lvc)
  | _, Dlit f, Dlit f' when f = f' -> (Dlit f, Val, [])
  | _ -> (Dite (c, t, e), Val, lve @ lvt @ lvc)

let build_transitive_composition ?(safe_def = false)
    ({ def_test; value_comp } : expression_composition) =
  (* `safe_def` can be set on call when we are sure that `value_comp` will
     always happen to be zero when `def_test` ends up false. E.g. arithmetic
     operation have such semantic property (funny question is what's the
     causality ?). This allows to remove a check to the definition flag when we
     compute the value, avoiding a lot of unnecessary code. *)
  let value_comp =
    if safe_def then value_comp else ite def_test value_comp (lit 0.)
  in
  { def_test; value_comp }

let build_expression (expr_comp : expression_composition) =
  let empty_stacks = { def_top = 0; val_top = 0; var_substs = [] } in
  let empty_locals = [] in
  ( collapse_constr empty_stacks empty_locals expr_comp.def_test,
    collapse_constr empty_stacks empty_locals expr_comp.value_comp )

let format_slot fmt ({ kind; depth } : stack_slot) =
  let kind = match kind with Def -> "int" | Val -> "real" in
  Format.fprintf fmt "%s%d" kind depth

let format_expr_var (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) fmt (ev : expr_var) =
  match ev with
  | Local slot -> format_slot fmt slot
  | M (var, offset, df) ->
      let def_flag = df = Def in
      Format.fprintf fmt "%s"
        (generate_variable ~debug_flag:dgfip_flags.flg_trace vm offset ~def_flag
           var)

let rec format_dexpr (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) fmt (de : expr) =
  let format_dexpr = format_dexpr dgfip_flags vm in
  match de with
  | Dtrue -> Format.fprintf fmt "1"
  | Dfalse -> Format.fprintf fmt "0"
  | Dlit f -> (
      match Float.modf f with
      | 0., _ ->
          (* Print at least one decimal, distinction from integers *)
          Format.fprintf fmt "%.1f" f
      | _ ->
          (* Print literal floats as precisely as possible *)
          Format.fprintf fmt "%#.19g" f)
  | Dvar evar -> format_expr_var dgfip_flags vm fmt evar
  | Dand (de1, de2) ->
      Format.fprintf fmt "@[<hov 2>(%a@ && %a@])" format_dexpr de1 format_dexpr
        de2
  | Dor (de1, de2) ->
      Format.fprintf fmt "@[<hov 2>(%a@ || %a@])" format_dexpr de1 format_dexpr
        de2
  | Dunop (op, de) -> Format.fprintf fmt "@[<hov 2>(%s%a@])" op format_dexpr de
  | Dbinop (op, de1, de2) ->
      Format.fprintf fmt "@[<hov 2>(%a@ %s %a@])" format_dexpr de1 op
        format_dexpr de2
  | Dfun (funname, des) ->
      Format.fprintf fmt "@[<hov 2>%s(%a@])" funname
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           format_dexpr)
        des
  | Daccess (var, dflag, de) ->
      Format.fprintf fmt "(%s[(int)%a])"
        (generate_variable ~def_flag:(dflag = Def)
           ~debug_flag:dgfip_flags.flg_trace vm PassPointer var)
        format_dexpr de
  | Dite (dec, det, dee) ->
      Format.fprintf fmt "@[<hov 2>(%a ?@ %a@ : %a@])" format_dexpr dec
        format_dexpr det format_dexpr dee

let format_local_vars_defs (dgfip_flags : Dgfip_options.flags)
    (vm : Dgfip_varid.var_id_map) fmt (lv : local_vars) =
  let lv = List.rev lv in
  let _ =
    List.fold_left
      (fun seen (_, { slot; _ }) ->
        if List.mem slot seen then seen
        else begin
          Format.fprintf fmt "@[<hov 2>%s %a;@]@,"
            (match slot.kind with Def -> "int" | Val -> "double")
            format_slot slot;
          slot :: seen
        end)
      [] lv
  in
  let format_one_assign fmt (_, { slot; subexpr }) =
    Format.fprintf fmt "@[<hov 2>%a =@ %a;@]@," format_slot slot
      (format_dexpr dgfip_flags vm)
      subexpr
  in
  List.iter (format_one_assign fmt) lv

let format_assign (dgfip_flags : Dgfip_options.flags)
    (var_indexes : Dgfip_varid.var_id_map) (var : string) fmt
    ((e, _kind, lv) : t) =
  Format.fprintf fmt "%a@[<hov 2>%s =@ %a;@]"
    (format_local_vars_defs dgfip_flags var_indexes)
    lv var
    (format_dexpr dgfip_flags var_indexes)
    e
