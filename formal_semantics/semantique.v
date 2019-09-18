(* Written using Coq 8.8.2 and coq-flocq 3.2.0 *)
From Flocq Require Import Core IEEE754.Binary IEEE754.Bits.
From Coq Require Import Strings.String.
From Coq Require Import Init.Datatypes.
Open Scope list_scope.


(*************************************************************)
(* This part is just small definitions about floating point. *)
(* No real floating-point property is used in the rest.      *)
(* This could be axiomatized                                 *)
(*************************************************************)
Definition fadd := b64_plus mode_NE.
Definition fsub := b64_minus mode_NE.
Definition fmult := b64_mult mode_NE.
Definition fdiv := b64_div mode_NE.
Definition fopp := b64_opp.
Definition fzero : binary64 := B754_zero 53 1024 false. (* or true? *)
Definition fabs := b64_abs.
Definition flt f1 f2 :=
  match b64_compare f1 f2 with
  | Some Lt => true
  | _ => false
  end.
Definition feq f1 f2 :=
  match b64_compare f1 f2 with
  | Some Eq => true
  | _ => false
  end.
Definition fgt f1 f2 :=
  match b64_compare f1 f2 with
  | Some Gt => true
  | _ => false
  end.
Definition fmin f1 f2 :=
  if flt f1 f2 then f1 else f2.
Definition fmax f1 f2 :=
  if flt f1 f2 then f2 else f1.

(* Similar to CompCert's implementation *)
Definition ZofB64 (f: binary64): option Z :=
  match f with
  | B754_finite _ _ s m (Zpos e) _ => Some (cond_Zopp s (Zpos m) * Z.pow radix2 (Zpos e))%Z
  | B754_finite _ _ s m (Zneg e) _ => Some (cond_Zopp s (Zpos m) / Z.pow radix2 (Zpos e))%Z
  | B754_zero _ _ b => Some 0%Z
  | _ => None
  end.

Let prec := 53%Z.
Let emax := 1024%Z.
Let Hprec : (0 < 53)%Z.
Proof.
apply refl_equal.
Qed.

Let Hprec_emax : (53 < 1024)%Z.
Proof.
apply refl_equal.
Qed.

Definition B64ofZ (z:Z) := binary_normalize prec emax Hprec Hprec_emax mode_NE z 0 false.

Definition is_integer (f: binary64) :=
  match f with
  | B754_finite _ _ s m (Zpos e) _ => true
  | B754_finite _ _ s m (Zneg e) _ => (((Zpos m) mod Z.pow radix2 (Zpos e)) =? 0)%Z
  | B754_zero _ _ b => true
  | _ => false
  end.

Definition valid_int_between (f: binary64) (l: Z) (u: Z) :=
  andb (is_integer f) (match ZofB64 f with
  | Some fz => andb (l <=? fz)%Z (fz <? u)%Z
  | None => false
  end).

(* checks that f is an integer, and between 0 and u *)
Definition valid_integer_access (f: binary64) (u: N) : option nat :=
  if is_integer f then
    match ZofB64 f with
    | Some fz => if (andb (0 <=? fz)%Z (fz <? (Z.of_N u))%Z) then Some (Z.to_nat fz) else None
    | _ => None
    end
  else None.

Lemma valid_integer_access_spec : forall f s n,
  valid_integer_access f s = Some n -> n < (N.to_nat s).
Proof.
  intros.
  unfold valid_integer_access in H.
  destruct (is_integer f); try discriminate.
  destruct (ZofB64 f); try discriminate.
  destruct ((0 <=? z)%Z && (z <? Z.of_N s)%Z)%bool eqn:Eq; try discriminate.
  rewrite Bool.andb_true_iff in Eq; destruct Eq.
  rewrite Z.ltb_lt in H1.
  inversion H; subst.
  rewrite Z.leb_le in H0.
  pose proof (Z2Nat.inj_lt z (Z.of_N s) H0).
  assert (0 <= Z.of_N s)%Z.
  { destruct s; simpl; eauto.
    + apply Z.le_refl.
    + apply Pos2Z.is_nonneg.
  }
  specialize (H2 H3).
  destruct H2 as [? _].
  specialize (H2 H1).
  assert (Z.to_nat (Z.of_N s) = N.to_nat s) by (destruct s; eauto).
  rewrite <- H4.
  apply H2.
Qed.


(****************************************)
(* Definition of the AST of M programs. *)
(****************************************)

Definition variable := string.
Definition table_size := N.


Inductive type :=
| TBool
| TFloat.

Inductive envtypes :=
| TScalar : type -> envtypes
| TTable : type -> envtypes.

Inductive values :=
| Bool : bool ->  values
| Float : binary64 -> values
| Undef : values.

Inductive envvalues :=
| VScalar : values -> envvalues
| VTable : list values -> table_size -> envvalues.

(* Contrary to the paper, we separated usual binary operators from
functions *)
Inductive logicop := | And | Or.

Inductive compop :=
| Lt
| Eq.

Inductive arithop :=
| Add
| Sub
| Mul
| Div.

(* Functions are handled in an ad-hoc manner to simplify the
formalization and due to the finite and small number of defined
functions *)
(* Functions of arity 1 *)
Inductive func1 :=
| Round | Inf | Abs | Gtzero | Gtezero
| Null | Present.

(* Functions of arity 2 *)
Inductive func2 :=
| Min | Max.



Inductive expression :=
| Value : values -> expression
| If : expression -> expression -> expression -> expression
| X : expression
| Var : variable -> expression
| UNeg : expression -> expression
| Logic : logicop -> expression -> expression -> expression
| Comp : compop -> expression -> expression -> expression
| Arith : arithop -> expression -> expression -> expression
| FunCall1 : func1 -> expression -> expression
| FunCall2 : func2 -> expression -> expression -> expression
| TableAccess : variable -> expression -> expression.

(* this corresponds to the errors of the verification condition *)
Inductive error := Error.

Inductive  command :=
| Assign : variable -> expression -> command
| TableAssign : variable -> table_size -> expression -> command
| Verif : expression -> error -> command.


Definition program := list command.


(*************************************************)
(* Definition of typing and evaluation contexts  *)
(*************************************************)
Definition partial_map (A : Type) := string -> (option A).

Definition upd_map (A: Type) (k:string) (v: A) (map: partial_map A) :=
  fun x => if string_dec k x then Some v else map x.

Definition environment := partial_map envvalues.
(* cmd_environment : None is error, Some is a regular environment *)
Definition cmd_environment := option environment.

Definition type_environment := partial_map envtypes.



(****************************************************)
(* Semantics of some functions and binary operators *)
(****************************************************)

Fixpoint add (l:values) (r:values) : option values :=
  match l, r with
  | Undef, Undef => Some Undef
  | Undef, Float f2 => Some (Float f2)
  | Float f1, Undef => Some (Float f1)
  | Float f1, Float f2  => Some (Float (fadd f1 f2))
  | _, _ => None
  end.

Fixpoint sub (l:values) (r:values) : option values :=
  match l, r with
  | Undef, Undef => Some Undef
  | Undef, Float f2 => Some (Float (fopp f2))
  | Float f1, Undef => Some (Float f1)
  | Float f1, Float f2  => Some (Float (fsub f1 f2))
  | _, _ => None
  end.

Fixpoint mul (l:values) (r:values) : option values :=
  match l, r with
  | Undef, Undef => Some Undef
  | Undef, Float f2 => Some (Float fzero)
  | Float f1, Undef => Some (Float fzero)
  | Float f1, Float f2  => Some (Float (fmult f1 f2))
  | _, _ => None
  end.

Fixpoint div (l: values) (r: values) : option values :=
  match l, r with
  | Undef, Undef => Some Undef
  | Float f1, Undef => Some Undef
  | Float f1, Float (B754_zero _ _ _) => Some Undef
  | Undef, Float f2 => Some (Float fzero)
  | Float f1, Float f2 => Some (Float (fdiv f1 f2))
  | _, _ => None
  end.

Fixpoint and (l: values) (r: values) : option values :=
  match l, r with
  | Undef, _ => Some Undef
  | _, Undef => Some Undef
  | Bool b1, Bool b2 => Some (Bool (andb b1 b2))
  | _, _ => None
  end.

Fixpoint or (l: values) (r: values) : option values :=
  match l, r with
  | Undef, _ => Some Undef
  | _, Undef => Some Undef
  | Bool b1, Bool b2 => Some (Bool (orb b1 b2))
  | _, _ => None
  end.

Fixpoint lt (l: values) (r: values) : option values :=
  match l, r with
  | Undef, _ => Some Undef
  | _, Undef => Some Undef
  | Float f1, Float f2 => Some (Bool (flt f1 f2))
  | _, _ => None
  end.

Fixpoint eq (l: values) (r: values) : option values :=
  match l, r with
  | Undef, _ => Some Undef
  | _, Undef => Some Undef
  | Float f1, Float f2 => Some (Bool (feq f1 f2))
  | _, _ => None
  end.

Fixpoint scalar_of_envvalue (e : envvalues) : option values :=
  match e with
  | VScalar v => Some v
  | _ => None
  end.

(********************************)
(* Semantics of the expressions *)
(********************************)
Fixpoint eval (Omega:environment) (e:expression) : option values :=
  match e with
  | Value v => Some v
  | If cond t f =>
    match eval Omega cond with
    | Some (Bool true) => eval Omega t
    | Some (Bool false) => eval Omega f
    | Some (Undef) => Some Undef
    | _ => None
    end
  | X =>
    match Omega "X"%string with
    | None => Some Undef
    | Some e => scalar_of_envvalue e
    end
  | Var v =>
    match Omega v with
    | None => Some Undef
    | Some e => scalar_of_envvalue e
    end
  | UNeg e =>
    match eval Omega e with
    | Some (Bool b) => Some (Bool (negb b))
    | Some Undef => Some Undef
    | _ => None
    end
  | Logic o e1 e2 =>
    let ee1 := eval Omega e1 in
    let ee2 := eval Omega e2 in
    match ee1, ee2 with
    | Some v1, Some v2 =>
      match o with
      | And => and v1 v2
      | Or => or v1 v2
      end
    | _, _ => None
    end
  | Comp c e1 e2 =>
    let ee1 := eval Omega e1 in
    let ee2 := eval Omega e2 in
    match ee1, ee2 with
    | Some v1, Some v2 =>
      match c with
      | Lt => lt v1 v2
      | Eq => eq v1 v2
      end
    | _, _ => None
    end
  | Arith op e1 e2 =>
    let ee1 := eval Omega e1 in
    let ee2 := eval Omega e2 in
    match ee1, ee2 with
    | Some v1, Some v2 => match op with
                          | Add => add v1 v2
                          | Sub => sub v1 v2
                          | Mul => mul v1 v2
                          | Div => div v1 v2
                          end
    | _, _ => None
    end
  | FunCall1 Round arg1 =>
    match eval Omega arg1 with
    | Some (Float f) => Some (Float f)
    | Some Undef => Some (Float fzero)
    | _ => None
    end
  | FunCall1 Inf arg1 =>
    match eval Omega arg1 with
    | Some (Float f) => Some (Float f)
    | Some Undef => Some (Float fzero)
    | _ => None
    end
  | FunCall1 Abs arg1 =>
    match eval Omega arg1 with
    | Some (Float f) => Some (Float (fabs f))
    | Some Undef => Some Undef
    | _ => None
    end
  | FunCall1 Gtzero arg1 =>
    match eval Omega arg1 with
    | Some (Float f) => Some (Bool (fgt f fzero))
    | Some Undef => Some Undef
    | _ => None
    end
  | FunCall1 Gtezero arg1 =>
    match eval Omega arg1 with
    | Some (Float f) => Some (Bool (orb (fgt f fzero) (feq f fzero)))
    | Some Undef => Some Undef
    | _ => None
    end
  | FunCall1 Null arg1 =>
    match eval Omega arg1 with
    | Some Undef => (* ??? *) Some (Bool true)
    | Some (Float f) => Some (Bool (feq f fzero))
    | Some (Bool b) => Some (Bool (Bool.eqb b false))
    | _ => None
    end
  | FunCall1 Present arg1 =>
    match eval Omega arg1 with
    | Some Undef => Some (Bool false)
    | Some _ => Some (Bool true)
    | _ => None
    end
  | FunCall2 Min arg1 arg2 =>
    match eval Omega arg1, eval Omega arg2 with
    | Some (Float f1), Some (Float f2) => Some (Float (fmin f1 f2))
    | Some Undef, _ => Some Undef
    | _, Some Undef => Some Undef
    | _, _ => None
    end
  | FunCall2 Max arg1 arg2 =>
    match eval Omega arg1, eval Omega arg2 with
    | Some (Float f1), Some (Float f2) => Some (Float (fmax f1 f2))
    | Some Undef, _ => Some Undef
    | _, Some Undef => Some Undef
    | _, _ => None
    end
  | TableAccess var index =>
    match eval Omega index with
    | Some (Float f) =>
      match Omega var with
      | Some (VTable values size) =>
        match valid_integer_access f size with
        | Some index =>
          Some (List.nth index values Undef)
        | None => Some Undef
        end
      | Some (VScalar _) => None
      | None => Some Undef
      end
    | Some Undef =>
      Some Undef
    | _ => None
    end
  end.




(************************************)
(* Typing jugement for expressions  *)
(************************************)
Inductive well_typed : type_environment -> expression -> type -> Prop :=
| WTBool : forall Gamma b, well_typed Gamma (Value (Bool b)) TBool
| WTFloat : forall Gamma f, well_typed Gamma (Value (Float f)) TFloat
| WTUndef : forall Gamma tau, well_typed Gamma (Value Undef) tau
| WTVar : forall Gamma x tau, Gamma(x) = Some (TScalar tau) -> well_typed Gamma (Var x) tau
| WTVarUndef : forall Gamma x tau, Gamma(x) = None -> well_typed Gamma (Var x) tau
| WTComp : forall Gamma compop e1 e2, well_typed Gamma e1 TFloat ->
                                 well_typed Gamma e2 TFloat ->
                                 well_typed Gamma (Comp compop e1 e2) TBool
| WTLogic : forall Gamma logop e1 e2, well_typed Gamma e1 TBool ->
                                 well_typed Gamma e2 TBool ->
                                 well_typed Gamma (Logic logop e1 e2) TBool
| WTUneg : forall Gamma e, well_typed Gamma e TBool -> well_typed Gamma (UNeg e) TBool
| WTIf : forall Gamma cond tr fa tau, well_typed Gamma cond TBool ->
                           well_typed Gamma tr tau ->
                           well_typed Gamma fa tau ->
                           well_typed Gamma (If cond tr fa) tau
| WTArith : forall Gamma op e1 e2, well_typed Gamma e1 TFloat ->
                             well_typed Gamma e2 TFloat ->
                             well_typed Gamma (Arith op e1 e2) TFloat
| WTIndex : forall Gamma tabvar index tabtau,
    Gamma(tabvar) = Some (TTable tabtau) ->
    well_typed Gamma index TFloat ->
    well_typed Gamma (TableAccess tabvar index) tabtau
| WTFuncFloat1Bool :
    forall Gamma f arg,
      f = Gtzero \/ f = Gtezero ->
      well_typed Gamma arg TFloat ->
      well_typed Gamma (FunCall1 f arg) TBool
| WTFuncFloat1Float :
    forall Gamma f arg,
      f = Round \/ f = Inf \/ f = Abs ->
      well_typed Gamma arg TFloat ->
      well_typed Gamma (FunCall1 f arg) TFloat
| WTFuncFloat2 :
    forall Gamma f arg1 arg2,
      f = Min \/ f = Max ->
      well_typed Gamma arg1 TFloat ->
      well_typed Gamma arg2 TFloat ->
      well_typed Gamma (FunCall2 f arg1 arg2) TFloat
| WTFuncTop1Bool :
    forall Gamma tau f arg,
      f = Null \/ f = Present ->
      well_typed Gamma arg tau ->
      well_typed Gamma (FunCall1 f arg) TBool.

Inductive value_of_type : values -> type -> Prop :=
| VoBool : forall b, value_of_type (Bool b) TBool
| VoFloat : forall f, value_of_type (Float f) TFloat
| VoUndef : forall tau, value_of_type Undef tau.

Hint Constructors value_of_type.

(***********************************************************************************)
(* These definitions specify how type and evaluation environnements can be related *)
(***********************************************************************************)
Definition related_envs_some_scalar (Gamma: type_environment) (Omega: environment) :=
  forall (s: string) ,
  forall tau, Gamma s = Some (TScalar tau) ->
         exists v, Omega s = Some (VScalar v) /\ value_of_type v tau.

Definition related_envs_some_tbl (Gamma: type_environment) (Omega: environment) :=
  forall (s: string),
    forall tau, Gamma s = Some (TTable tau) -> exists v size, Omega s = Some (VTable v size) /\ List.Forall (fun el => value_of_type el tau) v /\ (N.to_nat size) = List.length v.
(* I don't know if we need size = size', or if we can prove size = size' or sth... *)

Definition related_envs_none (Gamma: type_environment) (Omega: environment) :=
  forall (s: string), Gamma s = None -> Omega s = None.


Definition value_of_type_sound :
  forall v tau Gamma, value_of_type v tau -> well_typed Gamma (Value v) tau.
Proof.
  intros ? ? ? VoT; inversion VoT; subst; constructor; eauto.
Qed.


(***************************************************)
(* Soundness theorem for the typing of expressions *)
(***************************************************)
Theorem soundness_expr :
  forall Gamma Omega, related_envs_some_scalar Gamma Omega ->
                 related_envs_some_tbl Gamma Omega ->
                 related_envs_none Gamma Omega ->
                 forall e tau, well_typed Gamma e tau -> exists v, eval Omega e = Some v /\ value_of_type v tau.
Proof.
  intros Gamma Omega rGOSS rGOST rGON.
  induction e; intros ; inversion H; subst; simpl; eauto.
  - destruct (IHe1 TBool H4) as [v [IHe1e IHe1vt]]; clear IHe1; rewrite IHe1e.
    destruct v.
    + destruct b; eauto.
    + inversion IHe1vt.
    + eauto.
  - destruct (rGOSS _ _ H2) as [v0 [O_eq v0_ty]]; rewrite O_eq; simpl; eauto.
  - specialize (rGON _ H2); rewrite rGON; eauto.
  - destruct (IHe _ H2) as [v [O_eq v_ty]]; rewrite O_eq; simpl.
    inversion v_ty; subst; eauto.
  - destruct (IHe1 _ H5) as [v1 [IHe1v IHe1t]]; clear IHe1; rewrite IHe1v.
    destruct (IHe2 _ H6) as [v2 [IHe2v IHe2t]]; clear IHe2; rewrite IHe2v.
    inversion IHe1t as [? | f1 | ? ]; subst;
      inversion IHe2t as [? | f2 | ?]; subst; destruct l; simpl; eauto.
  - destruct (IHe1 _ H5) as [v1 [IHe1v IHe1t]]; clear IHe1; rewrite IHe1v.
    destruct (IHe2 _ H6) as [v2 [IHe2v IHe2t]]; clear IHe2; rewrite IHe2v.
    inversion IHe1t as [? | f1 | ? ]; subst;
      inversion IHe2t as [? | f2 | ?]; subst; destruct c; simpl; eauto.
  - destruct (IHe1 _ H5) as [v1 [IHe1v IHe1t]]; clear IHe1; rewrite IHe1v.
    destruct (IHe2 _ H6) as [v2 [IHe2v IHe2t]]; clear IHe2; rewrite IHe2v.
    inversion IHe1t as [? | f1 | ? ]; subst;
      inversion IHe2t as [? | f2 | ?]; subst; destruct a; simpl; eauto.
    destruct f2; eauto.
  - destruct (IHe _ H5) as [v [O_eq v_ty]]; rewrite O_eq; simpl.
    destruct H3; subst; inversion v_ty; eauto.
  - destruct (IHe _ H5) as [v [O_eq v_ty]]; rewrite O_eq; simpl.
    destruct H3 as [? | [? | ?]]; subst; inversion v_ty; eauto.
  - destruct (IHe _ H5) as [v [O_eq v_ty]]; rewrite O_eq; simpl.
    destruct H3; subst; inversion v_ty; eauto.
  - destruct (IHe1 _ H6) as [v1 [IHe1v IHe1t]]; clear IHe1; rewrite IHe1v.
    destruct (IHe2 _ H7) as [v2 [IHe2v IHe2t]]; clear IHe2; rewrite IHe2v.
    inversion IHe1t as [? | f1 | ? ]; subst;
      inversion IHe2t as [? | f2 | ?]; subst; destruct H4; subst; simpl; eauto.
  - destruct (IHe _ H5) as [ve [O_eq v_ty]]; rewrite O_eq; simpl.
    inversion v_ty; subst; eauto.
    specialize (rGOST _ _ H3).
    destruct rGOST as [v0 [s0 [Ov0_eq [Altau v0_eq]]]].
    rewrite Ov0_eq.
    destruct (valid_integer_access f _) eqn:VIA.
    + rewrite List.Forall_forall in Altau.
      pose proof valid_integer_access_spec _ _ _ VIA.
      rewrite v0_eq in H0.
      pose proof (List.nth_In _ Undef H0).
      specialize (Altau _ H1).
      eauto.
    + eauto.
Qed.

Require Import String.
Open Scope string_scope.

(* A helper function to define iteration during evaluation of table assignation *)
(* The first calls starts with exec_tbl Omega bound e bound nil *)
(* Assignments are executed from 0 to bound, but we use pos in a decreasing order to simplify the definition *)
Fixpoint exec_tbl (Omega:environment) (bound : nat) (e: expression) (pos: nat) (acc: list values) : option (list values) :=
  let Omega' := upd_map _ "X"%string (VScalar (Float (B64ofZ (Z.of_nat (bound - pos))))) Omega in
  match pos with
  | 0 =>
    match eval Omega' e with
    | Some v => Some (List.rev (v :: acc))
    | None => None
    end
  | S n' =>
    match eval Omega' e with
    | Some v => exec_tbl Omega bound e n' (v :: acc)
    | None => None
    end
  end.

(***********************************)
(* Semantics of command evaluation *)
(***********************************)
Fixpoint exec (Omega:cmd_environment) (c:command) : option cmd_environment :=
  match Omega with
  | Some Omega =>
    match c with
    | Assign x e =>
      match eval Omega e with
      | None => None
      | Some v => Some (Some (upd_map _ x (VScalar v) Omega))
      end
    | TableAssign x  tbl_size e =>
      match tbl_size with
      | N0 => None
      | Npos p =>
        match exec_tbl Omega (N.to_nat (Pos.pred_N p)) e (N.to_nat (Pos.pred_N p)) nil with
        | Some vals => Some (Some (upd_map _ x (VTable vals tbl_size) Omega))
        | None => None
        end
      end
    | Verif expr err =>
      match eval Omega expr with
      | Some (Bool true) => Some None (* error but valid omega*)
      | Some (Bool false) | Some Undef => Some (Some Omega)
      | _ => None
      end
    end
  | None (* meaning error *) => Some None
  end.

(*************************)
(* Semantics of programs *)
(*************************)
Fixpoint exec_program (Omega: cmd_environment) (p: program) : option cmd_environment :=
  match p with
  | nil => Some Omega
  | c :: p' => match exec Omega c with
             | Some Omega => exec_program Omega p'
             | None => None
             end
  end.

(*********************************************)
(* Typing judgment for commands and programs *)
(*********************************************)

Inductive well_typed_cmd : type_environment -> command -> type_environment -> Prop :=
| WTCAssign : forall Gamma var expr tau, well_typed Gamma expr tau -> well_typed_cmd Gamma (Assign var expr) (upd_map _ var  (TScalar tau) Gamma)
| WTCAssignTable : forall Gamma var tabsize expr tau,
    tabsize <> N0 ->
    well_typed (upd_map _ "X"%string (TScalar TFloat) Gamma) expr tau -> well_typed_cmd Gamma (TableAssign var tabsize expr) (upd_map _ var (TTable tau) Gamma)
| WTCCond : forall Gamma cond err, well_typed Gamma cond TBool -> well_typed_cmd Gamma (Verif cond err) Gamma.

Inductive well_typed_prog : type_environment -> program -> type_environment -> Prop :=
| WTPnil : forall Gamma, well_typed_prog Gamma nil Gamma
| WTPcons : forall Gamma c p' Gamma' Gamma'', well_typed_cmd Gamma c Gamma' -> well_typed_prog Gamma' p' Gamma'' -> well_typed_prog Gamma (c :: p') Gamma''.

(******************************************************************)
(* Related environments lifted to evaluation context for commands *)
(******************************************************************)
Definition related_envs Gamma Omega :=
  Omega = None \/
  exists Omega', Omega = Some Omega' /\ related_envs_some_scalar Gamma Omega' /\ related_envs_some_tbl Gamma Omega' /\ related_envs_none Gamma Omega'.

Hint Unfold related_envs.

(* Some administrative lemmas about the conversation of related_envs through updates  *)
Lemma upd_related_envs_scalar_table :
  forall Gamma Omega v tau vals size,
    related_envs_some_scalar Gamma Omega ->
    related_envs_some_scalar (upd_map envtypes v (TTable tau) Gamma) (upd_map envvalues v (VTable vals size) Omega).
Proof.
  intros.
  intros s tau' Hup.
  unfold upd_map in *.
  destruct (string_dec v s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_table_table :
  forall Gamma Omega v tau vals size,
    related_envs_some_tbl Gamma Omega ->
    List.Forall (fun el => value_of_type el tau) vals ->
    (N.to_nat size) = List.length vals ->
    related_envs_some_tbl (upd_map envtypes v (TTable tau) Gamma) (upd_map envvalues v (VTable vals size) Omega).
Proof.
  intros.
  intros ? ? Hup.
  unfold upd_map in *.
  destruct (string_dec v s); subst.
  - inversion Hup; subst; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_none_table :
  forall Gamma Omega v tau vals size,
    related_envs_none Gamma Omega ->
    related_envs_none (upd_map envtypes v (TTable tau) Gamma) (upd_map envvalues v (VTable vals size) Omega).
Proof.
  intros.
  intros s Hup.
  unfold upd_map in *.
  destruct (string_dec v s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_scalar_scalar :
  forall Gamma Omega v tau k,
    related_envs_some_scalar Gamma Omega ->
    value_of_type v tau ->
    related_envs_some_scalar (upd_map _ k (TScalar tau) Gamma) (upd_map _ k (VScalar v) Omega).
Proof.
  intros.
  intros s tau' Hup.
  unfold upd_map in *.
  destruct (string_dec _ s); subst.
  - inversion Hup; subst; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_table_scalar :
  forall Gamma Omega v tau k,
    related_envs_some_tbl Gamma Omega ->
    value_of_type v tau ->
    related_envs_some_tbl (upd_map _ k (TScalar tau) Gamma) (upd_map _ k (VScalar v) Omega).
Proof.
  intros.
  intros s tau' Hup.
  unfold upd_map in *.
  destruct (string_dec _ s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_none_scalar :
  forall Gamma Omega v tau k,
    related_envs_none Gamma Omega ->
    value_of_type v tau ->
    related_envs_none (upd_map _ k (TScalar tau) Gamma) (upd_map _ k (VScalar v) Omega).
Proof.
  intros.
  intros s Hup.
  unfold upd_map in *.
  destruct (string_dec _ s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

(* A lemma specifying the behavior of exec_tbl *)
Lemma exec_tbl_acc:
  forall Gamma Omega e tau bound,
    related_envs_some_scalar Gamma Omega ->
    related_envs_some_tbl Gamma Omega ->
    related_envs_none Gamma Omega ->
    well_typed (upd_map _ "X"%string (TScalar TFloat) Gamma) e tau ->
    forall n acc,
    bound >= n ->
    exists vals, exec_tbl Omega bound e n acc = Some (List.rev acc ++ vals)%list /\ List.length vals = S n /\
            (forall v, List.In v vals -> value_of_type v tau).
Proof.
  intros ? ? ? ? ? ? ? ?; induction n; intros acc Bn; intros.
  - simpl.
    remember (upd_map envvalues "X"%string (VScalar (Float (B64ofZ (Z.of_nat (bound - 0))))) Omega) as Omega'.
    remember (upd_map envtypes "X"%string (TScalar TFloat) Gamma) as Gamma'.
    assert (related_envs_some_scalar Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_scalar_scalar).
    assert (related_envs_some_tbl Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_table_scalar).
    assert (related_envs_none Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_none_scalar).
    pose proof (soundness_expr _ _ H3 H4 H5 _ _ H2) as HeqEe.
    destruct HeqEe as [v [Heqve Tauv]].
    rewrite Heqve.
    exists (v::nil); repeat split; simpl; eauto.
    intros ? [? | ?].
    + subst; eauto.
    + destruct H6.
  - simpl.
    remember (upd_map envvalues "X"%string (VScalar (Float (B64ofZ (Z.of_nat (bound - S n))))) Omega) as Omega'.
    remember (upd_map envtypes "X"%string (TScalar TFloat) Gamma) as Gamma'.
    assert (related_envs_some_scalar Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_scalar_scalar).
    assert (related_envs_some_tbl Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_table_scalar).
    assert (related_envs_none Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_none_scalar).
    pose proof (soundness_expr _ _ H3 H4 H5 _ _ H2) as HeqEe.
    destruct HeqEe as [v [Heqve Tauv]].
    rewrite Heqve.
    assert (bound >= n) by omega.
    specialize (IHn (v::acc) H6).
    destruct IHn as [vals [Heqvals [Lenvals Tauvals]]].
    exists (v::vals); repeat split; simpl; eauto.
    + rewrite Heqvals.
      f_equal.
      simpl.
      now rewrite <- List.app_assoc.
    + intros ? [? | ?]; subst; eauto.
Qed.


(**********************************)
(* Soundness theorem for commands *)
(**********************************)
Theorem soundness_cmd :
  forall Gamma Omega c Gamma',
    well_typed_cmd Gamma c Gamma' ->
    related_envs Gamma Omega ->
    exists Omega', exec Omega c = Some Omega' /\ (related_envs Gamma' Omega').
Proof.
  intros; destruct c; destruct Omega as [Omega | ]; simpl; inversion H; subst; eauto; destruct H0; try discriminate;
    destruct H0 as [Omega' [Oeq [rss [rst rn]]]]; inversion Oeq; subst;
  rename Omega' into Omega.
  - destruct (soundness_expr _ _ rss rst rn _ _ H5) as [ve [eval_eq_ve ve_tau]]; rewrite eval_eq_ve; simpl.
    exists (Some (upd_map _ v (VScalar ve) Omega)); repeat split; eauto.
    right; exists (upd_map _ v (VScalar ve) Omega); repeat split; eauto.
    + now apply upd_related_envs_scalar_scalar.
    + now apply upd_related_envs_table_scalar.
    + now apply upd_related_envs_none_scalar.
  - destruct t; [now destruct H6 |].
    assert (N.to_nat (Pos.pred_N p) >= N.to_nat (Pos.pred_N p)) by omega.
    pose proof (exec_tbl_acc Gamma Omega e tau _ rss rst rn H7 _ nil H0).
    destruct H1 as [vals [Heqvals [Lenvals Tauvals]]].
    rewrite Heqvals.
    exists (Some (upd_map envvalues v (VTable vals (Npos p)) Omega)); split; f_equal; eauto.
    right.
    exists (upd_map envvalues v (VTable vals (Npos p)) Omega); repeat split.
    + now apply upd_related_envs_scalar_table.
    + apply upd_related_envs_table_table; eauto.
      * now apply List.Forall_forall.
      * rewrite Lenvals.
        clear Gamma v e tau H Omega Oeq rss rst rn H6 H7 H0 vals Heqvals Lenvals Tauvals.
        simpl.
        pose proof (Pos.succ_pred_or p).
        destruct H as [? | ?]; subst; eauto.
        rewrite <- H.
        rewrite Pos.pred_N_succ.
        simpl.
        Require Import Coq.micromega.Lia.
        lia.
    + now apply upd_related_envs_none_table.
  - destruct (soundness_expr _ _ rss rst rn _ _ H5) as [ve [eval_eq_ve ve_tau]]; rewrite eval_eq_ve; simpl.
    inversion ve_tau; subst.
    + destruct b; eauto.
      exists (Some Omega); split; eauto.
      right; eauto.
    + exists (Some Omega); split; eauto.
      right; eauto.
Qed.

(**********************************)
(* Soundness theorem for programs *)
(**********************************)
Theorem soundness_prog :
  forall p Gamma Omega Gamma',
    well_typed_prog Gamma p Gamma' ->
    related_envs Gamma Omega ->
    exists Omega', exec_program Omega p = Some Omega' /\ (related_envs Gamma' Omega').
Proof.
  induction p; intros; inversion H; subst; simpl.
  - eauto.
  - pose proof soundness_cmd Gamma Omega a Gamma'0 H4 H0 as sound_a.
    destruct sound_a as [Omega'0 [HeqOmega'0 reOmega'0]].
    rewrite HeqOmega'0.
    eapply IHp; eauto.
Qed.

Theorem soundness_prog_starting_from_scratch:
  forall p Gamma,
    well_typed_prog (fun _ => None) p Gamma ->
    exists Omega, exec_program (Some (fun _ => None)) p = Some Omega /\ related_envs Gamma Omega.
Proof.
  intros.
  eapply soundness_prog; eauto.
  right.
  exists (fun _ => None); repeat split.
  - intros ? ? ?.
    inversion H0.
  - intros ? ? ?.
    inversion H0.
Qed.
