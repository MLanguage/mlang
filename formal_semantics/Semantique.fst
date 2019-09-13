module Semantique

module VInt = FStar.Int31
module Map = FStar.Map

assume new type m_float : eqtype

assume val m_float_zero : m_float

assume val m_float_add : m_float -> m_float -> m_float
assume val m_float_sub : m_float -> m_float -> m_float
assume val m_float_div : m_float -> m_float -> m_float
assume val m_float_mul : m_float -> m_float -> m_float
assume val m_float_gt : m_float -> m_float -> bool
assume val m_float_gte : m_float -> m_float -> bool
assume val m_float_lt : m_float -> m_float -> bool
assume val m_float_lte : m_float -> m_float -> bool
assume val m_float_modf : m_float -> (m_float & VInt.t)
assume val m_float_from_int : VInt.t -> m_float

type typ =
  | TBool
  | TFloat

type size = s:VInt.t{VInt.v s > 0}

assume val check_in_range (s: size) (f: m_float) : Lemma
  (requires (
    f `m_float_gte` m_float_zero /\
    f `m_float_lt` (m_float_from_int s) /\
    (let (frac, _) = m_float_modf f in frac = m_float_zero)
  ))
  (ensures (let (_, i) = m_float_modf f in VInt.v i >= 0 /\ VInt.v i < VInt.v s))

type var_typ =
  | Simple : typ -> var_typ
  | Table : typ -> size -> var_typ

type var_t = nat

type value =
  | Bool : (v: bool) -> value
  | Float : (v: m_float) -> value
  | Undef : value

type var_value =
  | VSimple : value -> var_value
  | VTable : (size:size) -> a:Seq.lseq value (VInt.v size) -> var_value

let of_type (v: value) (t: typ) : bool = match (v, t) with
  | (Bool _, TBool)
  | (Float _, TFloat)
  | (Undef, _) -> true
  | _ -> false

type logicop =
  | And
  | Or

type compop =
  | Lt
  | Lte
  | Gt
  | Gte
  | Neq
  | Eq

type arithop =
  | Add
  | Mul
  | Divi
  | Sub

type expression =
  | Value : value -> expression
  | If : expression -> expression -> expression -> expression
  | Var : var_t -> expression
  | Index : var_t -> expression -> expression
  | Logic : logicop -> expression -> expression -> expression
  | Comp : compop -> expression -> expression -> expression
  | GenericIndex : expression
  | Arith : arithop  -> expression -> expression -> expression

let var_map (a: Type) = Map.t var_t (option a)

type index = i:VInt.t{VInt.v i >= 0}

let zero_idx: (i:index{VInt.v i = 0}) = VInt.int_to_t (FStar.Int.zero 31)

noeq type value_environment = {
  v_vars: var_map var_value;
  v_generic_index: option index
}

let add_func (l r: value) : option value = match l,r with
  | Undef, Undef -> Some Undef
  | Undef, Float v2 -> Some (Float v2)
  | Float v1, Undef -> Some (Float v1)
  | Float v1, Float v2 -> Some (Float (v1 `m_float_add` v2))
  | _, _ -> None

let sub_func (l r: value) : option value = match l,r with
  | Undef, Undef -> Some Undef
  | Undef, Float v2 -> Some  (Float (m_float_zero `m_float_sub` v2))
  | Float v1, Undef -> Some (Float v1)
  | Float v1, Float v2 -> Some (Float (v1 `m_float_sub` v2))
  | _, _ -> None

let mul_func (l r: value) : option value = match l,r with
  | Undef, Undef -> Some Undef
  | Undef, Float v2 -> Some (Float m_float_zero)
  | Float v1, Undef ->  Some (Float m_float_zero)
  | Float v1, Float v2 -> Some (Float (v1 `m_float_mul` v2))
  | _, _ -> None

let div_func (l r: value) : option value = match l,r with
  | Undef, Undef -> Some Undef
  | Undef, Float v2 -> Some (Float m_float_zero)
  | Float v1, Undef ->  Some Undef
  | Float v1, Float v2 ->
    if v2 = m_float_zero then Some Undef else Some (Float (v1 `m_float_div` v2))
  | _, _ -> None


let and_func (l r: value) : option value = match l,r with
  | Undef, Undef -> Some Undef
  | Undef, Bool _
  | Bool _, Undef -> Some (Bool false)
  | Bool v1, Bool v2 -> Some (Bool (v1 && v2))
  | _, _ -> None

let or_func (l r: value) : option value = match l,r with
  | Undef, Undef -> Some Undef
  | Undef, Bool v2 -> Some (Bool v2)
  | Bool v1, Undef -> Some (Bool v1)
  | Bool v1, Bool v2 -> Some (Bool (v1 || v2))
  | _, _ -> None


let rec eval_expr ('w : value_environment) (e: expression) : option value = match e with
  | Value v -> Some v
  | If cond t f -> begin match eval_expr 'w cond with
    | Some (Bool true) -> eval_expr 'w t
    | Some (Bool false) -> eval_expr 'w f
    | Some Undef -> Some Undef
    | _ -> None
  end
  | Var var -> begin match Map.sel ('w).v_vars var with
    | Some (VSimple v) -> Some v
    | Some (VTable _ _) -> None
    | None -> Some Undef
  end
  | Arith op e1 e2 -> begin
    let new_e1 = eval_expr 'w e1 in
    let new_e2 = eval_expr 'w e2 in
    match (op, new_e1, new_e2) with
      | (Add, Some v1, Some v2) -> add_func v1 v2
      | (Sub, Some v1, Some v2) -> sub_func v1 v2
      | (Mul, Some v1, Some v2) -> mul_func v1 v2
      | (Divi, Some v1, Some v2) -> div_func v1 v2
      | _ -> None
  end
  | Comp op e1 e2 -> begin
    let new_e1 = eval_expr 'w e1 in
    let new_e2 = eval_expr 'w e2 in
    match (op, new_e1, new_e2) with
      | (Lt, Some (Float v1), Some (Float v2)) -> Some (Bool (v1 `m_float_lt` v2))
      | (Lte, Some (Float v1), Some (Float v2)) -> Some (Bool (v1 `m_float_lte` v2))
      | (Gt, Some (Float v1), Some (Float v2)) -> Some (Bool (v1 `m_float_gt` v2))
      | (Gte, Some (Float v1), Some (Float v2)) -> Some (Bool (v1 `m_float_gte` v2))
      | (Neq, Some (Float v1), Some (Float v2)) -> Some (Bool (v1 = v2))
      | (Eq, Some (Float v1), Some (Float v2)) -> Some (Bool (v1 <> v2))
      | (_, Some Undef, _)
      | (_, _, Some Undef) -> Some Undef
      | _ -> None
  end
  | Logic op e1 e2 -> begin
    let new_e1 = eval_expr 'w e1 in
    let new_e2 = eval_expr 'w e2 in
    match (op, new_e1, new_e2) with
      | (And, Some v1, Some v2) -> and_func v1 v2
      | (Or, Some v1, Some v2) -> or_func v1 v2
      | _ -> None
  end
  | Index var e -> begin
    let new_e = eval_expr 'w e in
    match (new_e, Map.sel ('w).v_vars var) with
    | (Some (Float x), Some (VTable size vs)) ->
      let (fraction, truncated) = m_float_modf x in
      if
        (fraction = m_float_zero) &&
        (x `m_float_lt` (m_float_from_int size)) &&
        (x `m_float_gte` m_float_zero)
      then begin
         check_in_range size x;
         Some (Seq.index vs (VInt.v truncated))
      end else Some Undef
    | (Some Undef, Some (VTable _ _)) ->
      Some Undef
    | _ -> None
  end
  | GenericIndex -> begin match ('w).v_generic_index with
    | Some i -> Some (Float (m_float_from_int i))
    | None -> None
  end

noeq type typ_environment = {
  t_vars: var_map var_typ;
  t_inside_table: bool;
}

let rec well_typed_expr ('c : typ_environment) (e: expression) (t: typ) : bool = match (e, t) with
  | (Value (Bool _), TBool)
  | (Value (Float _), TFloat)
  | (Value Undef, _) -> true
  | (If cond t f, typ) ->
    well_typed_expr 'c cond TBool &&
    well_typed_expr 'c t typ &&
    well_typed_expr 'c f typ
  | (Var var, typ) ->
    Map.sel ('c).t_vars var = Some (Simple typ) || Map.sel ('c).t_vars var = None
    (* This case is for undefined variables *)
  | (Arith op e1 e2, TFloat) ->
    well_typed_expr 'c e1 TFloat &&
    well_typed_expr 'c e2 TFloat
  | (Comp op e1 e2, TBool) ->
    well_typed_expr 'c e1 TFloat &&
    well_typed_expr 'c e2 TFloat
  | (Logic op e1 e2, TBool) ->
    well_typed_expr 'c e1 TBool &&
    well_typed_expr 'c e2 TBool
  | (Index var e, typ) ->
    well_typed_expr 'c e TFloat &&
    begin match Map.sel ('c).t_vars var with
    | Some (Table typ' _) -> typ = typ'
    | _ -> false
    end
  | (GenericIndex, TFloat) -> ('c).t_inside_table
  | _ -> false

let synchronised_var_entries ('c: typ_environment) ('w: value_environment) (var: var_t) : Type =
  match Map.sel ('c).t_vars var with
  | None -> Map.sel ('w).v_vars var = Some (VSimple Undef)
  | Some (Simple t) -> exists (v: value).
    Map.sel ('w).v_vars var = Some (VSimple v) /\ (v `of_type` t)
  | Some (Table t s) -> exists  (vs: Seq.lseq value (VInt.v s)).
    Map.sel ('w).v_vars var = Some (VTable s vs) /\
    (forall (i:VInt.t{VInt.v i >= 0 /\ VInt.v i < VInt.v s}). ((Seq.index vs (VInt.v i)) `of_type` t))

let synchronised_envs ('c: typ_environment) ('w: value_environment) : Type =
  (('c).t_inside_table ==> (exists (i: index). ('w).v_generic_index = Some i)) /\
  (forall (var: var_t). synchronised_var_entries 'c 'w var)

let synchronised_envs_intro ('c: typ_environment) ('w: value_environment)
  (sync_index: unit -> Lemma
    (requires (let it = ('c).t_inside_table in it))
    (ensures (match ('w).v_generic_index with Some i -> True | None -> False))
  )
  (sync_undef: (var: var_t) -> Lemma
    (requires (match Map.sel ('c).t_vars var with None -> True | Some _ -> False))
    (ensures (match Map.sel ('w).v_vars var with Some (VSimple Undef) -> True | _ -> False))
  )
  (sync_simple: (var: var_t) -> (t: typ) -> Lemma
    (requires (match Map.sel ('c).t_vars var with Some (Simple t') -> t = t' | _ -> False))
    (ensures (match Map.sel ('w).v_vars var with Some (VSimple v) ->  (v `of_type` t) | _ -> False))
  )
  (sync_table: (var: var_t) -> (t: typ) -> (s: size) -> Lemma
    (requires (match Map.sel ('c).t_vars var with Some (Table t' s') -> t' = t /\ s' = s | _ -> False))
    (ensures (match Map.sel ('w).v_vars var with
      | Some (VTable s' vs) -> s' = s /\
        (forall (i:VInt.t{VInt.v i >= 0 /\ VInt.v i < VInt.v s}). ((Seq.index vs (VInt.v i)) `of_type` t))
      | _ -> False
    ))
  )
  : Lemma (synchronised_envs 'c 'w)
  =
  if ('c).t_inside_table then sync_index () else ();
  assert((('c).t_inside_table ==> (exists (i: index). ('w).v_generic_index = Some i)));
  let aux (var: var_t) : Lemma (synchronised_var_entries 'c 'w var) =
    match Map.sel ('c).t_vars var with
    | None -> sync_undef var
    | Some (Simple t) -> sync_simple var t
    | Some (Table t s) -> sync_table var t s
  in
  Classical.forall_intro aux

let well_executed_expr ('w: value_environment) (e: expression) (t: typ) =
  exists (v: value). eval_expr 'w e = Some v /\ v `of_type` t

let well_executed_expr_elim ('w: value_environment) (e: expression) (t: typ) (goal : Type0)
  (lemma: (v:value) -> Lemma
    (requires (v `of_type` t /\ (match eval_expr 'w e with Some v' -> v = v' | None -> False)))
    (ensures (goal))
  )
  : Lemma
    (requires (well_executed_expr 'w e t))
    (ensures (goal))
  =
  let pf : squash ( exists (v: value). eval_expr 'w e = Some v /\ v `of_type` t) = () in
  FStar.Classical.exists_elim goal pf (fun v -> lemma v)

let rec type_safety_expr
  ('c: typ_environment)
  ('w: value_environment)
  (e: expression)
  (t: typ)
  : Lemma
    (requires (well_typed_expr 'c e t /\ synchronised_envs 'c 'w))
    (ensures (well_executed_expr 'w e t))
  =
  match (e, t) with
  | (Value (Bool _), TBool)
  | (Value (Float _), TFloat)
  | (Value Undef, _) -> ()
  | (If cond t f, typ) ->
    type_safety_expr 'c 'w cond TBool;
    type_safety_expr 'c 'w t typ;
    type_safety_expr 'c 'w f typ
  | (Var var, typ) -> ()
  | (Arith op e1 e2, TFloat) ->
    type_safety_expr 'c 'w e1 TFloat;
    type_safety_expr 'c 'w e2 TFloat
  | (Comp op e1 e2, TBool) ->
    type_safety_expr 'c 'w e1 TFloat;
    type_safety_expr 'c 'w e2 TFloat
  | (Logic op e1 e2, TBool) ->
    type_safety_expr 'c 'w e1 TBool;
    type_safety_expr 'c 'w e2 TBool
  | (Index var e1, typ) ->
    type_safety_expr 'c 'w e1 TFloat;
    well_executed_expr_elim 'w e1 TFloat (well_executed_expr 'w e typ) (fun v ->
      match Map.sel ('c).t_vars var with
      | Some (Table typ' s') -> begin match (Map.sel ('w).v_vars var, v) with
        | (Some (VTable s'' vs), Float x) ->
	  let (fraction, truncated) = m_float_modf x in
	  if
            (fraction = m_float_zero) &&
            (x `m_float_lt` (m_float_from_int s'')) &&
            (x `m_float_gte` m_float_zero)
          then begin
            check_in_range s'' x
          end else ()
        | _ -> ()
      end
      | _ -> ()
    )
  | (GenericIndex, TFloat) -> ()

type command =
  | Assign : var_t -> expression -> command
  | AssignTable : var_t -> size -> expression -> command
  | Condition : expression -> command

let rec iter_table_assignment
  ('w: value_environment)
  (e: expression)
  (var: var_t)
  (s: size)
  (i:index{VInt.v i <= VInt.v s})
  (vs: option (Seq.lseq value (VInt.v s)))
  : Tot (option (Seq.lseq value (VInt.v s)))
  (decreases (VInt.v s - VInt.v i))
  =
  if i = s then vs else
    let v = eval_expr ({ 'w with v_generic_index = Some i}) e in
    let new_vs = iter_table_assignment 'w e var s VInt.(i +^ VInt.int_to_t 1) vs in
    match (new_vs, v) with
      | (Some vs, Some v) -> Some (Seq.upd vs (VInt.v i) v)
      | _ -> None

let eval_command ('w: value_environment) (cmd: command) : option value_environment = match cmd with
  | Assign var e ->
    let v = eval_expr ({ 'w with v_generic_index = None})  e in
    Some ({ 'w with
      v_vars = Map.upd ('w).v_vars var (match v with None -> None | Some v -> Some (VSimple v))
    })
  | AssignTable var s e ->
    let vs = iter_table_assignment
      'w
      e
      var
      s
      zero_idx
      (Some (Seq.init (VInt.v s) (fun _ -> Undef)))
    in
    let omega' = { 'w with
      v_vars = Map.upd ('w).v_vars var (match vs with None -> None | Some vs -> Some (VTable s vs))
    } in
    Some omega'
  | Condition e ->
    let v = eval_expr ({ 'w with v_generic_index = None}) e in
    match v with
    | Some (Bool _) -> Some 'w
    | Some Undef -> Some 'w
    | _ -> None

let well_typed_command ('c: typ_environment) (cmd: command) (new_gamma: typ_environment) : Type = match cmd with
  | Assign var e -> begin match Map.sel ('c).t_vars var with
    | None -> exists (t:typ). well_typed_expr 'c e t /\ ('c).t_inside_table = false /\
      (forall (var':var_t{var' <> var}). Map.sel ('c).t_vars var' = Map.sel new_gamma.t_vars var') /\
      Map.sel ('c).t_vars var = None /\
      Map.sel new_gamma.t_vars var = Some (Simple t) /\
      (new_gamma).t_inside_table = false
    | Some _ -> False
  end
  | AssignTable var s e -> begin match Map.sel ('c).t_vars var with
    | None -> exists (t:typ). well_typed_expr 'c e t /\ ('c).t_inside_table = true /\
      (forall (var':var_t{var' <> var}). Map.sel ('c).t_vars var' = Map.sel new_gamma.t_vars var') /\
      Map.sel ('c).t_vars var = None /\
      Map.sel new_gamma.t_vars var = Some (Table t s) /\
      (new_gamma).t_inside_table = true
    | Some _ -> False
  end
  | Condition e -> well_typed_expr 'c e TBool /\ ('c).t_inside_table = false /\
    (forall (var':var_t). Map.sel ('c).t_vars var' = Map.sel new_gamma.t_vars var') /\
    (new_gamma).t_inside_table = false

let well_executed_command ('w: value_environment) (new_gamma: typ_environment) (cmd: command) : Type =
  exists (omega': value_environment). eval_command 'w cmd == Some omega' /\
    synchronised_envs new_gamma omega'

let rec iter_table_assignment_type_safe
  ('w: value_environment)
  ('c: typ_environment)
  (e: expression)
  (t: typ)
  (var: var_t)
  (s: size)
  (i:index{VInt.v i <= VInt.v s})
  (vs: (Seq.lseq value (VInt.v s)))
  : Lemma
  (requires (well_typed_expr 'c e t /\ ('c).t_inside_table = true /\ synchronised_envs 'c 'w))
  (ensures (exists (vs': (Seq.lseq value (VInt.v s))).
    iter_table_assignment 'w e var s i (Some vs) = Some vs' /\
    (forall (j:VInt.t{VInt.v j >= VInt.v i /\ VInt.v j < VInt.v s}).
       ((Seq.index vs' (VInt.v j)) `of_type` t))
  ))
  (decreases (VInt.v s - VInt.v i))
  =
  if i = s then () else begin
    iter_table_assignment_type_safe 'w 'c e t var s VInt.(i +^ VInt.int_to_t 1) vs;
    let correct_omega = ({ 'w with v_generic_index = Some i}) in
    type_safety_expr 'c correct_omega e t
  end

let type_safety_command
  ('c : typ_environment)
  (new_gamma: typ_environment)
  ('w: value_environment)
  (cmd: command)
  : Lemma
    (requires (well_typed_command 'c cmd new_gamma /\ synchronised_envs 'c 'w))
    (ensures (well_executed_command 'w new_gamma cmd))
  = match cmd with
  | Assign var e ->
    let correct_omega = ({'w with v_generic_index = None}) in
    let correct_gamma = ({'c with t_inside_table = false}) in
    synchronised_envs_intro correct_gamma correct_omega
      (fun () -> ())
      (fun var -> ())
      (fun var t -> ())
      (fun var t s -> ());
    let pf: squash (
      exists (t: typ).
        well_typed_expr 'c e t /\ ('c).t_inside_table = false /\
        (forall (var':var_t{var' <> var}). Map.sel ('c).t_vars var' = Map.sel new_gamma.t_vars var') /\
        Map.sel ('c).t_vars var = None /\
        Map.sel new_gamma.t_vars var = Some (Simple t) /\
        (new_gamma).t_inside_table = false
      ) =
        ()
    in
    Classical.exists_elim
      (well_executed_command 'w new_gamma cmd)
      pf
      (fun t ->
        type_safety_expr correct_gamma correct_omega e t;
	well_executed_expr_elim correct_omega e t (well_executed_command 'w new_gamma cmd) (fun v ->
          let omega' = { 'w with
            v_vars = Map.upd ('w).v_vars var (Some (VSimple v))
          } in
	  synchronised_envs_intro new_gamma omega'
	    (fun () -> ())
            (fun var -> ())
            (fun var t -> ())
            (fun var t s -> ())
        )
      )
  | AssignTable var s e ->
    let correct_gamma = ({'c with t_inside_table = true }) in
    synchronised_envs_intro correct_gamma 'w
      (fun () -> ())
      (fun var -> ())
      (fun var t -> ())
      (fun var t s -> ());
    let pf: squash (
      exists (t: typ).
        well_typed_expr 'c e t /\ ('c).t_inside_table = true /\
        (forall (var':var_t{var' <> var}). Map.sel ('c).t_vars var' = Map.sel new_gamma.t_vars var') /\
        Map.sel ('c).t_vars var = None /\
        Map.sel new_gamma.t_vars var = Some (Table t s) /\
        (new_gamma).t_inside_table = true
      )
     =
     ()
    in
     Classical.exists_elim
      (well_executed_command 'w new_gamma cmd)
      pf
      (fun t ->
	let init = Seq.init (VInt.v s) (fun _ -> Undef) in
	let pf: squash (exists (vs': (Seq.lseq value (VInt.v s))).
          iter_table_assignment 'w e var s zero_idx (Some init) = Some vs' /\
	  (forall (j:VInt.t{VInt.v j >= VInt.v zero_idx /\ VInt.v j < VInt.v s}).
            ((Seq.index vs' (VInt.v j)) `of_type` t))
        ) =
	  iter_table_assignment_type_safe 'w correct_gamma e t var s zero_idx init
	in
	Classical.exists_elim (well_executed_command 'w new_gamma cmd) pf (fun vs ->
	  match eval_command 'w cmd with
	  | None -> ()
	  | Some omega' ->
            assume(omega' == { 'w with
              v_vars = Map.upd ('w).v_vars var (Some (VTable s vs))
            });
            synchronised_envs_intro new_gamma omega'
              (fun () -> ())
              (fun var' -> ())
              (fun var' t -> ())
              (fun var' t' s' -> if var' <> var then () else ())
        )
      )
  | Condition e ->
    let correct_omega = ({'w with v_generic_index = None}) in
    let correct_gamma = ({'c with t_inside_table = false}) in
    synchronised_envs_intro correct_gamma correct_omega
      (fun () -> ())
      (fun var -> ())
      (fun var t -> ())
      (fun var t s -> ());
    type_safety_expr correct_gamma correct_omega e TBool;
    let omega' = 'w in
    well_executed_expr_elim correct_omega e TBool (well_executed_command 'w new_gamma cmd) (fun v ->
      assert(eval_expr correct_omega e = Some v);
      assert(eval_command 'w cmd == Some omega');
      synchronised_envs_intro new_gamma omega'
        (fun () -> ())
        (fun var -> ())
        (fun var t -> ())
        (fun var t s -> ())
    )
