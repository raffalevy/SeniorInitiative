(* 

Construction of lists in terms of base types.

*)

Require Import HoTT.

Print list.

Local Open Scope nat_scope.

Module NList. Section NList.

Variable (T : Type).

Fixpoint nlist (n : nat) : Type := match n with
| 0 => Unit
| S m => T * nlist m
end.

Definition list : Type := {n | nlist n}.

Definition nil : list := (0 ; tt).
Definition cons : T -> list -> list := fun x l => match l with
  (n ; tail) => (n.+1 ; (x , tail))
end.

Lemma nil_unique : forall l : nlist 0, l = tt.
Proof.
  destruct l. reflexivity.
Defined.

(* Lemma list_rect : forall (P : list -> Type),
  P nil -> (forall a l, P l -> P (cons a l))
  -> forall l, P l.
Proof.
  intros P B H. destruct l as [n l].
  induction n.
  - simpl. rewrite (nil_unique l). assumption.
  - destruct l as [x tail]. apply (H x (n ; tail)).
    apply IHn.
Defined. *)

Definition list_rect : forall (P : list -> Type),
  P nil -> (forall a l, P l -> P (cons a l))
  -> forall l, P l := fun P B H l =>
    sig_rect P (fun n =>
      nat_rect (fun n => forall l, P (n ; l))
        (fun l1 => paths_rec tt (fun a => P (0; a)) B l1 (nil_unique l1)^)
        (fun n IHn l1 => prod_rect (fun a => P (n.+1 ; a)) 
          (fun x tail => H x (n ; tail) (IHn tail))
          l1)
        n
    ) l.

Definition length (l : list) : nat := match l with
  (n ; tail) => n
end.

End NList. End NList.

Section NListSpec.

Open Scope list_scope.

Variable T : Type.

Definition nlist_to_list : NList.list T -> list T.
Proof.
  apply (NList.list_rect).
  - exact nil.
  - intros x tail tail'.
    exact (x :: tail').
Defined.

Definition list_to_nlist : list T -> NList.list T.
Proof.
  apply list_rect.
  - exact (NList.nil _).
  - intros x tail tail'.
    exact (NList.cons _ x tail').
Defined.

Theorem nlist_spec : NList.list T <~> list T.
Proof.
  exists nlist_to_list.
  apply isequiv_biinv; split; exists list_to_nlist.
  - simpl. apply NList.list_rect. { reflexivity. }
    intros x tail H.
    assert (Q :
      nlist_to_list (NList.cons T x tail) =
      x :: nlist_to_list tail). { reflexivity. }
    rewrite Q.
    assert (Q1 :
      list_to_nlist (x :: nlist_to_list tail) =
      NList.cons _ x (list_to_nlist (nlist_to_list tail))
    ). { reflexivity. }
    rewrite Q1. rewrite H. reflexivity.
  - simpl. apply list_rect. { reflexivity. }
    intros x tail H.
    assert (Q :
      list_to_nlist (x :: tail) =
      NList.cons _ x (list_to_nlist tail)). { reflexivity. }
    rewrite Q.
    assert (Q1 :
      nlist_to_list (NList.cons T x (list_to_nlist tail))
      = x :: nlist_to_list (list_to_nlist tail)). { reflexivity. }
    rewrite Q1. rewrite H. reflexivity.
Qed.

End NListSpec.

Theorem nlist_equals_list `{Univalence} : NList.list = list.
Proof.
  apply path_forall. intros T.
  apply path_universe_uncurried.
  apply nlist_spec.
Qed.