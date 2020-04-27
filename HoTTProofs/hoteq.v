Require Import HoTT.

Module UipUnit.

Definition nu : forall (x y : Unit), x = y -> x = y
  := fun x y p => match x with
    tt => match y with
      tt => idpath
    end
  end.

Lemma nu_const : forall (x y : Unit) (p q : x = y), nu x y p = nu x y q.
Proof. easy. Qed.

Lemma nu_id: forall (x y : Unit) (p : x = y), nu x y p = p.
Proof.
  intros x y p. unfold nu. destruct p.
  destruct x. reflexivity.
Qed.

Theorem uip_unit : forall (x y : Unit) (p q : x = y), p = q.
Proof.
  intros x y p q.
  rewrite<- (nu_id x y p).
  rewrite<- (nu_id x y q).
  apply nu_const.
Qed.

End UipUnit.

Module UipAB.

Local Open Scope type_scope.

Inductive AB := A | B.
Check AB_rect.
Check transport.

Definition discrim_AB : A <> B := fun H =>
  let P := (fun x => AB_rect (fun _ => Type) True False x) in
  AB_rect P tt (transport P H tt) B.

Definition discrim_BA : B <> A := fun H => discrim_AB (inverse H).

Definition dec_AB : forall x y : AB, (x = y) + (x <> y) := fun x y =>
  AB_rect (fun x => (x = y) + (x <> y))
    (AB_rect (fun y => (A = y) + (A <> y)) (inl idpath) (inr discrim_AB) y)
    (AB_rect (fun y => (B = y) + (B <> y)) (inr discrim_BA) (inl idpath) y)
  x.

Definition nu : forall (x y : AB), x = y -> x = y := fun x y p =>
  sum_rect (fun _ => x = y)
    (fun q => q)
    (fun q => Empty_rect (fun _ => x = y) (q p))
    (dec_AB x y).

Lemma nu_const : forall (x y : AB) (p q : x = y), nu x y p = nu x y q.
Proof.
  destruct q; destruct x; reflexivity.
Qed.

Lemma nu_id: forall (x y : AB) (p : x = y), nu x y p = p.
Proof.
  destruct p; destruct x; reflexivity.
Qed.

Theorem uip_AB : forall (x y : AB) (p q : x = y), p = q.
Proof.
  intros x y p q.
  rewrite<- (nu_id x y p).
  rewrite<- (nu_id x y q).
  by apply nu_const.
Qed.

Local Close Scope type_scope.

End UipAB.

Module UipNat.

Local Open Scope nat_scope.

Definition nat_pair_rect :
  forall P : nat -> nat -> Type,
    (forall n, P n 0) -> (forall n, P 0 n)
    -> (forall a b, P a b -> P a.+1 b.+1)
    -> forall a b, P a b
:=
  fun P Bl Br H a =>
    nat_rect (fun a => forall b, P a b)
      (fun _ => Br _)
      (fun a IH1 b => nat_rect (fun b => P a.+1 b)
        (Bl _)
        (fun b IH2 => H a b (IH1 b))
      b)
      a.

Local Close Scope nat_scope.

Local Open Scope type_scope.

(* Definition discrim_OS : forall n, O <> S n:= fun n H =>
  let P := (fun x => nat_rect (fun _ => Type) True (fun _ _ => False) x) in
  nat_rect P tt (fun _ _ => transport P H tt) (S n). *)

Definition discrim_OS : forall n, O <> S n:= fun n H =>
  let P := (fun x => nat_rect (fun _ => Type) True (fun _ _ => False) x) in
  transport P H tt.

Definition discrim_SO : forall n, S n <> O := fun n H => discrim_OS n (inverse H).

Definition pred (n : nat) : nat := nat_rect _ 
  O (fun m _ => m) n.

Definition S_inj : forall (x y : nat), S x = S y -> x = y := 
  fun x y H => ap pred H.

Check sum_rect.
Definition dec_nat : forall x y : nat, (x = y) + (x <> y) := fun x y =>
  nat_pair_rect (fun x y => (x = y) + (x <> y))
    (fun x => nat_rect (fun x => (x = 0) + (x <> 0))
      (inl idpath) (fun n _ => inr (discrim_SO _)) x)
    (fun y => nat_rect (fun y => (0 = y) + (0 <> y))
      (inl idpath) (fun n _ => inr (discrim_OS _)) y)
    (fun x y IH => sum_rect (fun _ => (S x = S y) + (S x <> S y))
      (fun H => inl (ap S H))
      (fun H => inr (fun H1 => H (S_inj x y H1)))
    IH)
    x y.

Definition nu : forall (x y : nat), x = y -> x = y := fun x y p =>
  sum_rect (fun _ => x = y)
    (fun q => q)
    (fun q => Empty_rect (fun _ => x = y) (q p))
    (dec_nat x y).

Lemma dec_nat_x_x : forall x, dec_nat x x = inl idpath.
Proof.
  induction x.
  - reflexivity.
  - simpl. rewrite IHx. reflexivity.
Qed.

Lemma nu_const : forall (x y : nat) (p q : x = y), nu x y p = nu x y q.
Proof.
  destruct q.
  unfold nu. rewrite dec_nat_x_x. reflexivity.
Qed.

Lemma nu_id: forall (x y : nat) (p : x = y), nu x y p = p.
Proof.
  destruct p. unfold nu. rewrite dec_nat_x_x.
  reflexivity.
Qed.

Theorem uip_nat : forall (x y : nat) (p q : x = y), p = q.
Proof.
  intros x y p q.
  rewrite<- (nu_id x y p).
  rewrite<- (nu_id x y q).
  by apply nu_const.
Qed.

Local Close Scope type_scope.

End UipNat.

Module UipGen.

Context {T : Type}.

Definition dec_eq := forall x y : T, (x = y) + (x <> y).

Context (dec : dec_eq).

Definition nu : forall (x y : T), x = y -> x = y :=
  fun x y p =>
    sum_rect (fun _ => x = y)
      (fun q => q)
      (fun q => Empty_rect (fun _ => x = y) (q p))
      (dec x y).

Lemma dec_x_x : forall x, {p | dec x x = inl p}.
Proof.
  intros x.
  destruct (dec x x).
  - exists p. reflexivity.
  - apply Empty_rect. apply n. exact idpath.
Qed. 

Lemma nu_const : forall (x y : T) (p q : x = y), nu x y p = nu x y q.
Proof.
  destruct q.
  destruct (dec_x_x x) as [r H].
  unfold nu. rewrite H. simpl. reflexivity.
Qed.

Theorem uip : forall (x y : T) (p q : x = y), p = q.
Proof.
  intros x y p q.
  assert (Q : forall p : x = y, p = (nu _ _ idpath)^ @ nu _ _ p).
  - intros r. destruct r. symmetry. apply concat_Vp.
  - destruct q. rewrite (Q p).
    rewrite (nu_const _ _ p 1).
    apply concat_Vp.
Qed.

End UipGen.