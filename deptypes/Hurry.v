(* examples from "Coq in a Hurry" *)

Require Import Arith.

Lemma and_comm_impl : forall A B : Prop, A /\ B -> B /\ A.
(*Proof. intuition/tauto/firstorder. Qed.*)
Proof.
  intros A B H. split.
   destruct H as [H1 H2]. exact H2.
   destruct H as [H1 H2]. exact H1.
Qed.

Example le_3_5 : 3 <= 5.
Proof. apply le_S; apply le_S; apply le_n. Qed.

Example le_trans_10 :
  forall x y : nat, 
    (x <= 10) -> (10 <= y) -> (x <= y).
Proof.
 intros x y x10 y10.
 apply le_trans with (m := 10).
  exact x10.
  exact y10.
Qed.

Lemma fullsqr : forall x y, 
  (x + y) * (x + y) = (x*x + 2*x*y + y*y).
Proof.
  intros x y.
  (* use `SearchRewrite (_ * (_ + _))` to find this *)
  rewrite mult_plus_distr_l.  (* (x+y)*x+(x+y)*y = x*x+2*x*y+y*y *)
  rewrite mult_plus_distr_r; rewrite mult_plus_distr_r;
  rewrite plus_assoc.                     (* x*x+y*x+x*y+y*y = x*x+2*x*y+y*y *)
  rewrite <- plus_assoc with (n := x*x).  (* x*x+(y*x+x*y)+y*y = x*x+2*x*y+y*y *)
  (* use `SearchPattern (?x * ?y = ?y * ?x) *)
  rewrite mult_comm with (n := y).        (* x*x+(x*y+x*y)+y*y = ... *)
  (* `SearchRewrite (S _ * _) *)
  (* limit rewrite to the first occurrence *)
  pattern (x*y) at 1; rewrite <- mult_1_l. (* x*x+(1*(x*y)+x*y)+y*y = x*x+2*x*y+y*y *)
  rewrite <- mult_succ_l.                 (* x*x+2*(x*y)+y*y = ... *)
  rewrite mult_assoc with (n := 2).       (* equality *)
  reflexivity.
Qed.

(* Example tautology_1: forall p q r s : Prop, *)

(****** NUMBERS *******)
Fixpoint sum_n n :=
  match n with
    0 => 0
  | S p => p + sum_n p
  end.

Lemma sum_n_p : forall n, 2 * sum_n n + n = n * n.
Proof.
induction n.
 simpl. reflexivity.

 (* n: nat, IHn: 2 * sum_n n + n = n * n |= 2 * sum_n (S n) + S n = S n * S n *)
 assert (SnSn : S n * S n = n*n + 2*n + 1). ring.
 (* now SnSn is in the context *)
 rewrite SnSn.
 rewrite <- IHn. 
 simpl. (* compute sum_n (S n) symbolically *)
 (* n + sum_n n + (n + sum_n n + 0) + S n 
    = sum_n n + (sum_n n + 0) + n + (n + (n + 0)) + 1 *)
 ring. 
Qed.

Fixpoint evenb (n:nat): bool :=
  match n with
    0 => true
  | 1 => false
  | S (S n1) => evenb n1
  end.

Lemma evenb_p : forall n, evenb n = true -> exists x, n = 2 * x.
(*Proof.
 assert (Main: forall n,
     (evenb n = true -> exists x, n = 2 * x)
  /\ (evenb n = false -> exists x, S n = 2 * x)).
 induction n.
    (* subgoal 1:
        forall n, (evenb n = true -> exists x, n = 2 * x)
               /\ (evenb n = false -> exists x, S n = 2 * x)
     *)
    split.
     exists 0; ring. (* true -> true -> exists x, 0 = x + (x + 0) *)
     intros H; discriminate H. (* get rid of true = false -> ... *)
    (* subgoal 2: 
        forall n, (evenb n = true -> exists x, S n = 2 * x) 
               /\ (evenb n = false -> exists x, S (S n) = 2 * x)
    *)
    split.
     destruct IHn as [_ IHn']; exact IHn'.
Qed.
*)
