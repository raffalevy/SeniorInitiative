Require Import HoTT.

Open Scope nat_scope.
Open Scope type_scope.

Module Ind.

Inductive BTree :=
| L : BTree
| B : BTree -> BTree -> BTree.

End Ind.

Module NatPair.

Fixpoint nat_to_pair (n : nat) : nat * nat := match n with
| 0 => (0,0)
| S n => match nat_to_pair n with
  | (a, 0) => (0, S a)
  | (a, S n) => (S a, n)
  end
end.

Lemma pair_rect : forall P : nat -> nat -> Type,
  (forall x, P x 0) -> (forall x, P 0 x)
  -> (forall a b, P a b -> P a.+1 b.+1)
  -> forall a b, P a b.
Proof.
  intros P Hl Hr.
  induction a.
  - exact Hr.
  - destruct b.
    + apply Hl.
    + apply X. apply IHa.
Defined.

Section DiagRect.

Variable (P : nat -> nat -> Type).
Hypothesis  (Base : P 0 0)
            (H1 : forall n, P n 0 -> P 0 (S n))
            (H2 : (forall a b, P a (S b) -> P (S a) b)).

Definition leq (a b : nat) := a - b = 0.

Lemma leq_S : forall a b, leq a.+1 b -> leq a b.
Proof.
  unfold leq. intros a b.
  apply (pair_rect (fun a b => a.+1 - b = 0 -> a - b = 0)); clear a b.
  - destruct x; try easy. intros H.
    apply (ap pred) in H. apply H.
  - reflexivity.
  - easy.
Defined.

Lemma leq_x0 : forall x, leq x 0 -> x = 0.
Proof.
  induction x; try easy.
Defined.

Lemma leq_SS : forall a b, leq a.+1 b.+1 -> leq a b.
Proof.
  refine (pair_rect _ _ _ _); easy.
Defined.

Lemma leq_SS_inv : forall a b, leq a b -> leq a.+1 b.+1.
Proof.
  refine (pair_rect _ _ _ _); easy.
Defined.

Lemma leq_xS : forall a b, leq a b -> leq a b.+1.
Proof.
  induction a; try easy.
  intros b H. apply leq_SS_inv. apply leq_S. assumption.
Defined.

Lemma ne_S0 : forall x, x.+1 <> 0.
Proof.
  intros x H.
  simple refine (transport (fun n => match n with
  | 0 => Unit
  | S _ => Empty
  end) H^ tt).
Defined.

Lemma leq_0x : forall x, leq 0 x.
Proof. easy. Defined.

Definition leq_S0 : forall x, ~ leq x.+1 0 := ne_S0.

Lemma eq_SS : forall a b, S a = S b -> a = b.
Proof. intros a b H. apply (ap pred) in H. apply H. Defined.

Lemma leq_xx : forall x, leq x x.
Proof. induction x; easy. Defined.

Lemma ext_S : forall x a, leq a x -> (x.+1 - a) = (x - a).+1.
Proof.
  refine (pair_rect _ _ _ _); try easy.
  - intros x _. destruct x; easy.
  - intros x H. apply leq_x0 in H. rewrite H. reflexivity.
Defined. 

Lemma sub_along_diag : forall x a, leq a x -> P 0 x -> P a (x - a).
Proof.
  intros x a H.
  induction a.
  - destruct x; try easy.
  - destruct x.
    + assert (Q : a = 0).
      { apply leq_x0. apply leq_S. assumption. }
      rewrite Q. compute. intros _. apply H2. apply H1.
      assumption.
    + apply leq_SS in H.
      pose (IHa' := IHa (leq_xS _ _ H)).
      intros H3. apply IHa' in H3.
      apply H2. simpl.
      rewrite<- (ext_S x a H). apply H3.
Defined.

Lemma traverse_diag : forall x, P 0 x -> P x 0.
Proof.
  intros x H.
  pose (Q := sub_along_diag x x (leq_xx x) H).
  rewrite leq_xx in Q. assumption.
Defined.

Lemma diag_rect : forall a b, P a b.
Proof.
  induction a.
  + induction b; try easy.
    apply H1. apply traverse_diag. assumption.
  + intros b. apply H2. apply IHa.
Defined.

Lemma diag_rect_spec : forall n, diag_rect 0 n.+1 = H1 n (diag_rect n 0).
Proof.
  induction n; try easy.
  simpl in *. rewrite IHn. apply (ap _).
  unfold traverse_diag.

End DiagRectSpec.

End DiagRect.

Section DiagRectSpec.

Open Scope nat_scope.

Variable (P : nat -> nat -> Type).
Hypothesis  (Base : P 0 0)
            (H1 : forall n, P n 0 -> P 0 (S n))
            (H2 : (forall a b, P a (S b) -> P (S a) b)).

Lemma diag_rect_spec : forall n, diag_rect P Base H1 H2 0 n.+1 = H1 n (diag_rect P Base H1 H2 n 0).
Proof.
  apply (diag_rect (fun x n => forall n : nat, x = 0 -> diag_rect P Base H1 H2 x n.+1 = H1 n (diag_rect P Base H1 H2 n x))).

  induction n; try easy.
  simpl in *. rewrite IHn. apply (ap _).
  unfold traverse_diag.

End DiagRectSpec.

Definition pair_to_nat : nat -> nat -> nat := fun a b =>
  diag_rect (fun _ _ => nat) 0
    (fun _ fno => S fno)
    (fun a b pasb => S pasb) a b.

Definition pair_prod_to_nat : nat * nat -> nat := fun p =>
  let (a,b) := p in pair_to_nat a b.

Lemma pair_prod_OS : forall n, pair_prod_to_nat (0, n.+1) = (pair_prod_to_nat (n, 0)).+1.
Proof.
  induction n; try easy.
  unfold pair_prod_to_nat in *. simpl in *.
  unfold pair_to_nat in *.
  simpl.

Definition pair_nat_equiv : (nat * nat) <~> nat.
Proof.
  exists pair_prod_to_nat.
  apply isequiv_biinv; split; exists nat_to_pair; unfold Sect.
  - destruct x as [a b].
    apply (diag_rect
      (fun a b => nat_to_pair (pair_prod_to_nat (a, b)) = (a, b))
    ); try easy.
    + intros n H.
      assert (Q : pair_prod_to_nat (0, n.+1) = S (pair_prod_to_nat (n, 0))).
      { destruct n; try easy. simpl.
        simpl in IHn.
        
      unfold pair_prod_to_nat. unfold pair_to_nat.
        induction n. easy. unfold diag_rect. simpl.
      }


(* 

f 0 0 = 0
f 0 (S n) = S (f n 0)
f (S n) m = S (f n (S m))

*)
