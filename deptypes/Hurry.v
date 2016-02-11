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
