Section Sandbox.
  Check 0.
  Check nat.
  Check Set.
  Check Type.
  Check 2 + 2.
  Compute 2 + 2.
  Variable n : nat.
  Check n + 2.
  Compute n + 2.
  (* Definition plus2 (m : nat) := m + 2. *)
  Definition plus2 := fun (m : nat) => m + 2.
  Check plus2.
  Print plus2.
  Compute plus2 2.
  Check n > 2.
  Compute n > 2.
  Compute 3 <= n.
  Check 3 <= 5.
  Check Prop.
  Goal 3 <= 5.
  auto.
  Show Proof.
  Qed.
  Check le_n.
  Check le_S.
  Print le_n.
End Sandbox.

Section PropMinLog.
  Variable A B C : Prop.
  Goal A -> A.
    intro u.
    (* tauto. *)
    exact u.
    Show Proof.
    Save Identity.
  Print Identity.
  
  Definition Identity2 : A -> A := fun (u : A) => u.
  Print Identity2.

  Definition K : A -> B -> A := fun (u:A) (v:B) => u.
  Print K.
  
  Lemma Hilbert2 : A -> B -> A.
    intro u.
    intro v.
    exact u.
  Qed.
  
  Print Hilbert2.
  
  Definition S : (A -> B -> C) -> (A -> B) -> A -> C :=
    fun (u : A -> B -> C) (v : A -> B) (w : A) => u w (v w).
  
  Lemma Hilbert1 : (A -> B -> C) -> (A -> B) -> A -> C.
    intros u v w.
    apply u.
    apply w.
    apply v.
    apply w.
  Qed.
  
  Lemma ConjunctionIsCommutative : A /\ B -> B /\ A.
    intro u.
    split.
      (* Goal 1: B *)
      apply u.
      (* Goal 2: B *)
      apply u.
  Qed.
  
  Print ConjunctionIsCommutative.
  Print conj.
End PropMinLog.

Section PredMinLog.
  Variable α : Set.
  Variable c : α.
  Variable p : α -> Prop.
  Check p(c).
  Check p.
  
  Lemma ForallImpliesExists : (forall (x:α), p x) -> exists (x:α), p x.
    intro u.
    exists c.
    apply u.
  Qed.
  
  Variable R : α -> α -> Prop.
  
  Hypothesis RIsSymmetric : forall (x y : α), R x y -> R y x.
  Hypothesis RIsTransitive : forall (x y z : α), R x y -> R y z -> R x z.
  
  Theorem RIsReflexive : forall (x:α), ((exists(y:α), R x y) -> R x x).
    intros x exists_y_Rxy.
    elim exists_y_Rxy.
    intros a Rxa.
    apply RIsTransitive with (y := a).
      (* Goal 1: R x a *)
      exact Rxa.
      (* Goal 2: R a x *)
      apply RIsSymmetric.
      exact Rxa.
    Qed.
    
    Print RIsReflexive.
End PredMinLog.

Section PropClassicalLog.
  Hypothesis Stab : forall (A : Prop), ~~A -> A.
  
  Variable A B : Prop.
  
  Lemma LEM : A \/ ~A.
    apply Stab.
    intro u.
    apply u.
    right.
    intro v.
    apply u.
    left.
    exact v.
  Qed.
    
  Lemma Peirce : ((A -> B) -> A) -> A.
    intro u.
    apply Stab.
    intro v.
    apply v.
    apply u.
    intro w.
    exfalso.
    apply v.
    apply w.
  Qed.
    
  Lemma DeMorganLTR1 : ~(A /\ B) -> ~A \/ ~B.
    intro u.
    apply Stab.
    intro v.
    apply v.
    left.
    intro a.
    apply v.
    right.
    intro b.
    apply u.
    split.
      exact a.
      exact b.
    Qed.
    
End PropClassicalLog.

Section DrinkersFormula.
  Hypothesis Stab : forall (A : Prop), ~~A -> A.
  
  Variable bar : Set.
  Variable drinks : bar -> Prop.
  Variable drunkard : bar.
  
  Theorem DrinkersFormula :
    exists (person : bar), (drinks person -> forall (guy : bar), drinks guy).
    apply Stab.
    intro u.
    apply u.
    exists drunkard.
    intro drunkard_drinks.
    intro guy.
    apply Stab.
    intro guy_does_not_drink.
    apply u.
    exists guy.
    intro guy_drinks.
    exfalso.
    apply guy_does_not_drink.
    apply guy_drinks.
  Qed.
  
End DrinkersFormula.
