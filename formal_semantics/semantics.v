(* Written using Coq 8.12.0 and coq-flocq 3.3.1 *)
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
Definition fzero : binary64 := B754_zero 53 1024 false.

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
Definition fne f1 f2 :=
  match b64_compare f1 f2 with
  | Some Ne => true
  | _ => false
  end.
Definition fgt f1 f2 :=
  match b64_compare f1 f2 with
  | Some Gt => true
  | _ => false
  end.
Definition fge f1 f2 :=
  match b64_compare f1 f2 with
  | Some Ge => true
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

Definition fone := B64ofZ (1%Z).

Definition fandb f1 f2 :=
  andb (fne f1 fzero) (fne f2 fzero).

Definition forb f1 f2 :=
  orb (fne f1 fzero) (fne f2 fzero).

Definition float_of_bool (b:bool) :=
  if b then fone else fzero.


(* Definition is_integer (f: binary64) := *)
(*   match f with *)
(*   | B754_finite _ _ s m (Zpos e) _ => true *)
(*   | B754_finite _ _ s m (Zneg e) _ => (((Zpos m) mod Z.pow radix2 (Zpos e)) =? 0)%Z *)
(*   | B754_zero _ _ b => true *)
(*   | _ => false *)
(*   end. *)

(* Definition valid_int_between (f: binary64) (l: Z) (u: Z) := *)
(*   andb (is_integer f) (match ZofB64 f with *)
(*   | Some fz => andb (l <=? fz)%Z (fz <? u)%Z *)
(*   | None => false *)
(*   end). *)

(* checks that f is an integer, and between 0 and u *)
Definition valid_integer_access (f: binary64) (u: N) : option nat :=
    match ZofB64 f with
    | Some fz => if (andb (0 <=? fz)%Z (fz <? (Z.of_N u))%Z) then Some (Z.to_nat fz) else None
    | _ => None
    end.

Lemma valid_integer_access_spec : forall f s n,
  valid_integer_access f s = Some n -> n < (N.to_nat s).
Proof.
  intros.
  unfold valid_integer_access in H.
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



Inductive envtypes :=
| TScalar
| TTable.

Inductive values :=
| Float : binary64 -> values
| Undef : values.

Inductive envvalues :=
| VScalar : values -> envvalues
| VTable : list values -> table_size -> envvalues.


(* Functions are handled in an ad-hoc manner to simplify the
formalization and due to the finite and small number of defined
functions *)
(* Functions of arity 1 *)
Inductive func1 :=
| Round | Inf | Abs | Gtzero | Gtezero
| Null | Present.

(* Functions of arity 2 *)
Inductive func2 :=
| And | Or | Lt | Eq | Add | Sub | Mul | Div
| Min | Max.



Inductive expression :=
| Value : values -> expression
| If : expression -> expression -> expression -> expression
| X : expression
| Var : variable -> expression
| UNeg : expression -> expression
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

Definition add (l:values) (r:values) : values :=
  match l, r with
  | Undef, Undef =>  Undef
  | Undef, Float f2 => Float f2
  | Float f1, Undef => Float f1
  | Float f1, Float f2  => Float (fadd f1 f2)
  end.

Definition sub (l:values) (r:values) : values :=
  match l, r with
  | Undef, Undef => Undef
  | Undef, Float f2 => Float (fopp f2)
  | Float f1, Undef => Float f1
  | Float f1, Float f2  => Float (fsub f1 f2)
  end.

Definition mul (l:values) (r:values) : values :=
  match l, r with
  | Undef, _ | _, Undef => Undef
  | Float f1, Float f2  => Float (fmult f1 f2)
  end.

Definition div (l: values) (r: values) : values :=
  match l, r with
  | Undef, _ | _, Undef  => Undef
  | Float f1, Float (B754_zero _ _ _) => Float fzero
  | Float f1, Float f2 => Float (fdiv f1 f2)
  end.


Definition and (l: values) (r: values) : values :=
  match l, r with
  | Undef, _ | _, Undef  => Undef
  | Float f1, Float f2 => Float (float_of_bool (fandb f1 f2))
  end.

Definition or (l: values) (r: values) : values :=
  match l, r with
  | Undef, _ | _, Undef => Undef
  | Float f1, Float f2 => Float (float_of_bool (forb f1 f2))
  end.

Definition lt (l: values) (r: values) : values :=
  match l, r with
  | Undef, _ | _, Undef => Undef
  | Float f1, Float f2 => Float (float_of_bool (flt f1 f2))
  end.

Definition eq (l: values) (r: values) : values :=
  match l, r with
  | Undef, _ | _, Undef => Undef
  | Float f1, Float f2 => Float (float_of_bool (feq f1 f2))
  end.

Definition min (l: values) (r: values) : values :=
  match l, r with
  | Undef, Undef => Float fzero
  | Float f1, Undef => Float (fmin f1 fzero)
  | Undef, Float f2 => Float (fmin fzero f2)
  | Float f1, Float f2 => Float (fmin f1 f2)
  end.

Definition max (l: values) (r: values) : values :=
  match l, r with
  | Undef, Undef => Float fzero
  | Float f1, Undef => Float (fmax f1 fzero)
  | Undef, Float f2 => Float (fmax fzero f2)
  | Float f1, Float f2 => Float (fmax f1 f2)
  end.

Definition round (v : values) : values :=
  match v with
  | Undef => Undef
  | Float f =>
    (* FIXME: there should be a round here, but I haven't found the
    corresponding Flocq definition yet. We could always cast to
    integers and back... *)
    Float f
  end.

Definition inf (v: values) : values :=
  match v with
  | Undef => Undef
  (* FIXME: inf missing *)
  | Float f => Float f
  end.

Definition abs (v: values) : values :=
  match v with
  | Undef => Undef
  | Float f => Float (fabs f)
  end.


Definition gtzero (v: values) : values :=
  match v with
  | Undef => Undef
  | Float f => Float (float_of_bool (fgt f fzero))
  end.

Definition gtezero (v: values) : values :=
  match v with
  | Undef => Undef
  | Float f => Float (float_of_bool (fge f fzero))
  end.

Definition null (v: values) : values :=
  match v with
  | Undef => Undef
  | Float f => Float (float_of_bool (feq f fzero))
  end.

Definition present (v: values) : values :=
  match v with
  | Undef => Float (float_of_bool false)
  | Float f => Float (float_of_bool true)
  end.

Definition scalar_of_envvalue (e : envvalues) : option values :=
  match e with
  | VScalar v => Some v
  | _ => None
  end.

(********************************)
(* Semantics of the expressions *)
(********************************)
Fixpoint eval (Omega:environment) (e:expression) : option values :=
  match e with
  (* D-VALUE *)
  | Value v => Some v
  (* D-COND-{TRUE,FALSE,UNDEF} *)
  | If cond t f =>
    match eval Omega cond with
    | Some (Float c) =>
      if feq c fzero then eval Omega f else eval Omega t
    | Some Undef =>
      Some Undef
    | _ => None
    end
  (* D-X *)
  | X =>
    match Omega "X"%string with
    | None => Some Undef
    | Some e => scalar_of_envvalue e
    end
  (* D-VAR *)
  | Var v =>
    match Omega v with
    | None => Some Undef
    | Some e => scalar_of_envvalue e
    end
  (* hidden in D-FUNC *)
  | UNeg e =>
    match eval Omega e with
    | Some (Float f) => Some (Float (fsub fone f))
    | Some Undef => Some Undef
    | _ => None
    end
  (* D-FUNC *)
  | FunCall1 o arg1 =>
    match eval Omega arg1 with
    | Some v =>
      Some
        (match o with
         | Round => round v
         | Inf => inf v
         | Abs => abs v
         | Gtzero => gtzero v
         | Gtezero => gtezero v
         | Null => null v
         | Present => present v
         end
        )
    | _ => None
    end
  (* D-FUNC *)
  | FunCall2 o arg1 arg2 =>
    let ee1 := eval Omega arg1 in
    let ee2 := eval Omega arg2 in
    match ee1, ee2 with
    | Some v1, Some v2 =>
      Some
        (match o with
         | And => and v1 v2
         | Or  => or  v1 v2
         | Lt  => lt  v1 v2
         | Eq  => eq  v1 v2
         | Add => add v1 v2
         | Sub => sub v1 v2
         | Mul => mul v1 v2
         | Div => div v1 v2
         | Min => min v1 v2
         | Max => max v1 v2
         end)
    | _, _ => None
    end
  (* D-INDEX-NEG, D-INDEX-UNDEF, D-INDEX-OUTSIDE, D-TAB-UNDEF, D-INDEX *)
  | TableAccess var index =>
    match eval Omega index with
    | Some (Float f) =>
      match Omega var with
      | Some (VTable values size) =>
        if flt f fzero then Some (Float fzero)
        else
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
Inductive well_formed : type_environment -> expression -> Prop :=
| TFloat : forall Gamma f, well_formed Gamma (Value (Float f))
| TUndef : forall Gamma, well_formed Gamma (Value Undef)
| TVarUndef : forall Gamma x, Gamma(x) = None -> well_formed Gamma (Var x)
| TVar : forall Gamma x, Gamma(x) = Some TScalar -> well_formed Gamma (Var x)
| TIndexUndef : forall Gamma x e, Gamma(x) = None -> well_formed Gamma e -> well_formed Gamma (TableAccess x e)
| TIndex : forall Gamma tabvar index,
    Gamma(tabvar) = Some TTable ->
    well_formed Gamma index ->
    well_formed Gamma (TableAccess tabvar index)
| TConditional : forall Gamma cond tr fa,
    well_formed Gamma cond ->
    well_formed Gamma tr ->
    well_formed Gamma fa ->
    well_formed Gamma (If cond tr fa)
| Tfunc1 : forall Gamma f arg,
    well_formed Gamma arg ->
    well_formed Gamma (FunCall1 f arg)
| Tfunc2 : forall Gamma f a1 a2,
    well_formed Gamma a1 ->
    well_formed Gamma a2 ->
    well_formed Gamma (FunCall2 f a1 a2).

(* Hint Constructors value_of_type. *)

(***********************************************************************************)
(* These definitions specify how type and evaluation environnements can be related *)
(***********************************************************************************)
Definition related_envs_some_scalar (Gamma: type_environment) (Omega: environment) :=
  forall (s: variable), Gamma s = Some TScalar ->
                   exists v, Omega s = Some (VScalar v).

Definition related_envs_some_tbl (Gamma: type_environment) (Omega: environment) :=
  forall (s: string), Gamma s = Some TTable -> exists v size, Omega s = Some (VTable v size) /\ (N.to_nat size) = List.length v.

Definition related_envs_none (Gamma: type_environment) (Omega: environment) :=
  forall (s: string), Gamma s = None -> Omega s = None.


(***************************************************)
(* Soundness theorem for the typing of expressions *)
(***************************************************)
Theorem soundness_expr :
  forall Gamma Omega, related_envs_some_scalar Gamma Omega ->
                 related_envs_some_tbl Gamma Omega ->
                 related_envs_none Gamma Omega ->
                 forall e, well_formed Gamma e -> exists v, eval Omega e = Some v.
Proof.
  intros Gamma Omega rGOSS rGOST rGON.
  induction e; intros ; inversion H; subst; simpl; eauto.
  - destruct (IHe1 H4) as [v IHe1e]; rewrite IHe1e.
    destruct v.
    + destruct feq; eauto.
    + eauto.
  - specialize (rGON _ H2); rewrite rGON; eauto.
  - destruct (rGOSS _ H2) as [v0 O_eq]; rewrite O_eq; simpl; eauto.
  - destruct (IHe H2) as [v O_eq]; rewrite O_eq; simpl.
    destruct f; simpl; eauto.
  - destruct (IHe1 H3) as [v1 IHe1v]; clear IHe1; rewrite IHe1v.
    destruct (IHe2 H5) as [v2 IHe2v]; clear IHe2; rewrite IHe2v.
    destruct f; simpl; eauto.
  - destruct (IHe H4) as [ve O_eq]; rewrite O_eq; simpl.
    specialize (rGON _ H3); rewrite rGON.
    eexists Undef; destruct ve; eauto.
  - destruct (IHe H4) as [ve O_eq]; rewrite O_eq; simpl.
    specialize (rGOST _ H3).
    destruct rGOST as [v0 [s0 [Ov0_eq length]]].
    rewrite Ov0_eq.
    destruct ve; eauto.
    destruct flt; eauto.
    destruct (valid_integer_access _ _) eqn:VIA.
    + pose proof valid_integer_access_spec _ _ _ VIA.
      rewrite length in H0.
      pose proof (List.nth_In _ Undef H0).
      eauto.
    + eauto.
Qed.




Require Import String.
Require Import Coq.micromega.Lia.
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
    (* D-ASSIGN *)
    | Assign x e =>
      match eval Omega e with
      | None => None
      | Some v => Some (Some (upd_map _ x (VScalar v) Omega))
      end
    (* D-ASSIGN-TABLE *)
    | TableAssign x  tbl_size e =>
      match tbl_size with
      | N0 => None
      | Npos p =>
        match exec_tbl Omega (N.to_nat (Pos.pred_N p)) e (N.to_nat (Pos.pred_N p)) nil with
        | Some vals => Some (Some (upd_map _ x (VTable vals tbl_size) Omega))
        | None => None
        end
      end
    (* D-ASSERT-OTHER, D-ASSERT-TRUE *)
    | Verif expr err =>
      match eval Omega expr with
      | Some (Float f) =>
        if feq f fzero then Some (Some Omega) else Some None
      | Some Undef => Some (Some Omega)
      | _ => None
      end
    end
  (* D-ERROR *)
  | None (* meaning error *) => Some None
  end.

(*************************)
(* Semantics of programs *)
(*************************)
Fixpoint exec_program (Omega: cmd_environment) (p: program) : option cmd_environment :=
  match p with
  | nil => Some Omega
  (* D-SEQ *)
  | c :: p' => match exec Omega c with
             | Some Omega => exec_program Omega p'
             | None => None
             end
  end.

(*********************************************)
(* Typing judgment for commands and programs *)
(*********************************************)

Inductive well_formed_cmd : type_environment -> command -> type_environment -> Prop :=
| TCAssign : forall Gamma var expr,
    (Gamma var <> None -> Gamma var = Some TScalar) ->
    well_formed Gamma expr ->
    well_formed_cmd Gamma (Assign var expr) (upd_map _ var  TScalar Gamma)
| TCAssignArray : forall Gamma var tabsize expr,
    tabsize <> N0 ->
    (Gamma var <> None -> Gamma var = Some TTable) ->
    well_formed (upd_map _ "X"%string TScalar Gamma) expr ->
    well_formed_cmd Gamma (TableAssign var tabsize expr) (upd_map _ var TTable Gamma)
| TCCond : forall Gamma cond err,
    well_formed Gamma cond ->
    well_formed_cmd Gamma (Verif cond err) Gamma.

Inductive well_formed_prog : type_environment -> program -> type_environment -> Prop :=
| WTPnil : forall Gamma, well_formed_prog Gamma nil Gamma
| WTPcons : forall Gamma c p' Gamma' Gamma'',
    well_formed_cmd Gamma c Gamma' ->
    well_formed_prog Gamma' p' Gamma'' ->
    well_formed_prog Gamma (c :: p') Gamma''.

(******************************************************************)
(* Related environments lifted to evaluation context for commands *)
(******************************************************************)
Definition related_envs Gamma Omega :=
  Omega = None \/
  exists Omega', Omega = Some Omega' /\ related_envs_some_scalar Gamma Omega' /\ related_envs_some_tbl Gamma Omega' /\ related_envs_none Gamma Omega'.

Hint Unfold related_envs.

(* Some administrative lemmas about the conversation of related_envs through updates  *)
Lemma upd_related_envs_scalar_table :
  forall Gamma Omega v vals size,
    related_envs_some_scalar Gamma Omega ->
    related_envs_some_scalar (upd_map envtypes v TTable Gamma) (upd_map envvalues v (VTable vals size) Omega).
Proof.
  intros.
  intros s Hup.
  unfold upd_map in *.
  destruct (string_dec v s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_table_table :
  forall Gamma Omega v vals size,
    related_envs_some_tbl Gamma Omega ->
    (N.to_nat size) = List.length vals ->
    related_envs_some_tbl (upd_map envtypes v TTable Gamma) (upd_map envvalues v (VTable vals size) Omega).
Proof.
  intros.
  intros ? Hup.
  unfold upd_map in *.
  destruct (string_dec v s); subst.
  - inversion Hup; subst; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_none_table :
  forall Gamma Omega v vals size,
    related_envs_none Gamma Omega ->
    related_envs_none (upd_map envtypes v TTable Gamma) (upd_map envvalues v (VTable vals size) Omega).
Proof.
  intros.
  intros s Hup.
  unfold upd_map in *.
  destruct (string_dec v s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_scalar_scalar :
  forall Gamma Omega v k,
    related_envs_some_scalar Gamma Omega ->
    related_envs_some_scalar (upd_map _ k TScalar Gamma) (upd_map _ k (VScalar v) Omega).
Proof.
  intros.
  intros s Hup.
  unfold upd_map in *.
  destruct (string_dec _ s); subst.
  - inversion Hup; subst; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_table_scalar :
  forall Gamma Omega v k,
    related_envs_some_tbl Gamma Omega ->
    related_envs_some_tbl (upd_map _ k TScalar Gamma) (upd_map _ k (VScalar v) Omega).
Proof.
  intros.
  intros s Hup.
  unfold upd_map in *.
  destruct (string_dec _ s); subst.
  - inversion Hup; eauto.
  - now apply H.
Qed.

Lemma upd_related_envs_none_scalar :
  forall Gamma Omega v k,
    related_envs_none Gamma Omega ->
    related_envs_none (upd_map _ k TScalar Gamma) (upd_map _ k (VScalar v) Omega).
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
  forall Gamma Omega e bound,
    related_envs_some_scalar Gamma Omega ->
    related_envs_some_tbl Gamma Omega ->
    related_envs_none Gamma Omega ->
    well_formed (upd_map _ "X"%string TScalar Gamma) e ->
    forall n acc,
    bound >= n ->
    exists vals, exec_tbl Omega bound e n acc = Some (List.rev acc ++ vals)%list /\ List.length vals = S n.
Proof.
  intros ? ? ? ? ? ? ? ?; induction n; intros acc Bn; intros.
  - simpl.
    remember (upd_map envvalues "X"%string (VScalar (Float (B64ofZ (Z.of_nat (bound - 0))))) Omega) as Omega'.
    remember (upd_map envtypes "X"%string TScalar Gamma) as Gamma'.
    assert (related_envs_some_scalar Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_scalar_scalar).
    assert (related_envs_some_tbl Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_table_scalar).
    assert (related_envs_none Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_none_scalar).
    pose proof (soundness_expr _ _ H3 H4 H5 _ H2) as [v Heqve].
    rewrite Heqve.
    exists (v::nil); repeat split; simpl; eauto.
  - simpl.
    remember (upd_map envvalues "X"%string (VScalar (Float (B64ofZ (Z.of_nat (bound - S n))))) Omega) as Omega'.
    remember (upd_map envtypes "X"%string TScalar Gamma) as Gamma'.
    assert (related_envs_some_scalar Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_scalar_scalar).
    assert (related_envs_some_tbl Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_table_scalar).
    assert (related_envs_none Gamma' Omega') by (rewrite HeqOmega', HeqGamma'; now apply upd_related_envs_none_scalar).
    pose proof (soundness_expr _ _ H3 H4 H5 _ H2) as [v Heqve].
    rewrite Heqve.
    assert (bound >= n) by lia.
    specialize (IHn (v::acc) H6).
    destruct IHn as [vals [Heqvals Lenvals]].
    exists (v::vals); repeat split; simpl; eauto.
    + rewrite Heqvals.
      f_equal.
      simpl.
      now rewrite <- List.app_assoc.
Qed.


(**********************************)
(* Soundness theorem for commands *)
(**********************************)
Theorem soundness_cmd :
  forall Gamma Omega c Gamma',
    well_formed_cmd Gamma c Gamma' ->
    related_envs Gamma Omega ->
    exists Omega', exec Omega c = Some Omega' /\ (related_envs Gamma' Omega').
Proof.
  intros; destruct c; destruct Omega as [Omega | ]; simpl; inversion H; subst; eauto; destruct H0; try discriminate;
    destruct H0 as [Omega' [Oeq [rss [rst rn]]]]; inversion Oeq; subst;
  rename Omega' into Omega.
  - destruct (soundness_expr _ _ rss rst rn _ H6) as [ve eval_eq_ve]; rewrite eval_eq_ve; simpl.
    exists (Some (upd_map _ v (VScalar ve) Omega)); repeat split; eauto.
    right; exists (upd_map _ v (VScalar ve) Omega); repeat split; eauto.
    + now apply upd_related_envs_scalar_scalar.
    + now apply upd_related_envs_table_scalar.
    + now apply upd_related_envs_none_scalar.
  - destruct t; [now destruct H5 |].
    assert (N.to_nat (Pos.pred_N p) >= N.to_nat (Pos.pred_N p)) by lia.
    pose proof (exec_tbl_acc Gamma Omega e _ rss rst rn H8 _ nil H0).
    destruct H1 as [vals [Heqvals Lenvals]].
    rewrite Heqvals.
    exists (Some (upd_map envvalues v (VTable vals (Npos p)) Omega)); split; f_equal; eauto.
    right.
    exists (upd_map envvalues v (VTable vals (Npos p)) Omega); repeat split.
    + now apply upd_related_envs_scalar_table.
    + apply upd_related_envs_table_table; eauto.
      * rewrite Lenvals.
        simpl.
        pose proof (Pos.succ_pred_or p).
        destruct H1 as [? | ?]; subst; eauto.
        rewrite <- H1.
        rewrite Pos.pred_N_succ.
        simpl.
        lia.
    + now apply upd_related_envs_none_table.
  - destruct (soundness_expr _ _ rss rst rn _  H5) as [ve eval_eq_ve]; rewrite eval_eq_ve; simpl.
    destruct ve; subst.
    + destruct feq; eauto.
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
    well_formed_prog Gamma p Gamma' ->
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
    well_formed_prog (fun _ => None) p Gamma ->
    exists Omega, exec_program (Some (fun _ => None)) p = Some Omega /\ related_envs Gamma Omega.
Proof.
  intros.
  eapply soundness_prog; eauto.
  right.
  exists (fun _ => None); repeat split.
  - intros ? ?.
    inversion H0.
  - intros ? ?.
    inversion H0.
Qed.
