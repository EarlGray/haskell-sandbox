Require Import Bool Arith List. (* CpdtTactics. *)
Set Implicit Arguments.

Inductive binop : Set := Plus | Times.

Inductive exp : Set := 
 | Const : nat -> exp
 | Binop : binop -> exp -> exp -> exp.

Definition binopDenote (b : binop) : nat -> nat -> nat :=
 match b with
  | Plus => plus
  | Times => mult
 end.

Fixpoint expDenote (e : exp) : nat :=
 match e with
  | Const n => n
  | Binop b e1 e2 => (binopDenote b) (expDenote e1) (expDenote e2)
 end.

Inductive instr : Set :=
 | iConst : nat -> instr
 | iBinop : binop -> instr.

Definition prog := list instr.
Definition stack := list nat.

Definition instrDenote (i : instr) (s : stack) : option stack :=
 match i with
  | iConst n => Some (n :: s)
  | iBinop b => 
      match s with
        | arg1 :: arg2 :: s' => Some ((binopDenote b) arg1 arg2 :: s')
        | _ => None
      end
 end.

Fixpoint progDenote (p : prog) (s : stack) : option stack :=
 match p with
  | nil => Some s
  | i :: p' => 
      match instrDenote i s with
        | None => None
        | Some s' => progDenote p' s'
      end
 end. 

Fixpoint compile (e : exp) : prog :=
 match e with
  | Const n => iConst n :: nil
  | Binop b e1 e2 => compile e2 ++ compile e1 ++ iBinop b :: nil
 end.

Lemma compile_correct' : forall e p s,
  progDenote (compile e ++ p) s = progDenote p (expDenote e :: s).
Proof.
  induction e.
  intros.
  unfold compile.
  unfold expDenote.
  unfold progDenote at 1.
  simpl.
  fold progDenote.
  reflexivity.
  intros.
  unfold compile.
  fold compile.
  unfold expDenote.
  fold expDenote.
  rewrite app_assoc_reverse.
  rewrite IHe2.
  rewrite app_assoc_reverse.
  rewrite IHe1.
  simpl.
  reflexivity.
Qed.  

Theorem compile_correct : forall e,
  progDenote (compile e) nil = Some (expDenote e :: nil).
Proof.
  intros.
  rewrite (app_nil_end (compile e)).
  rewrite compile_correct'.
  reflexivity.
Qed.


(*****************               ***********************)

Inductive type : Set := Nat | Bool.
Inductive tbinop : type -> type -> type -> Set :=
 | TPlus : tbinop Nat Nat Nat
 | TTimes : tbinop Nat Nat Nat
 | TEq : forall t, tbinop t t Bool
 | TLt : tbinop Nat Nat Bool.

Inductive texp : type -> Set :=
 | TNConst : nat -> texp Nat
 | TBConst : bool -> texp Bool
 | TBinop : forall t1 t2 t, tbinop t1 t2 t -> texp t1 -> texp t2 -> texp t.

Definition typeDenote (t : type) : Set := 
 match t with
  | Nat => nat
  | Bool => bool
 end.

Definition tbinopDenote arg1 arg2 res (b: tbinop arg1 arg2 res)
            : typeDenote arg1 -> typeDenote arg2 -> typeDenote res 
            :=
 match b with
  | TPlus => plus
  | TTimes => mult
  | TEq Nat => beq_nat
  | TEq Bool => eqb
  | TLt => leb
 end.

Fixpoint texpDenote t (e : texp t) : typeDenote t :=
 match e with
  | TNConst n => n
  | TBConst b => b
  | TBinop _ _ _ b e1 e2 => (tbinopDenote b) (texpDenote e1) (texpDenote e2)
 end.
