variable (A B C : Prop)

theorem I : A -> A := fun (u : A) => u

theorem I2 : A -> A := by
 intro u
 exact u

theorem K : A -> B -> A := fun (u : A) (v : B) => u

theorem K2 : A -> B -> A := by
 intro u v
 exact u

theorem S : (A -> B -> C) -> (A -> B) -> A -> C :=
 fun (u : A -> B -> C) (v : A -> B) (w : A) => u w (v w)

theorem S2  : (A -> B -> C) -> (A -> B) -> A -> C := by
 intro u v w
 apply u
 . apply w
 . apply v
   apply w

variable (p : Nat -> Prop)

theorem ForallImpliesExists: (∀ (x : Nat), p x) -> ∃ (x : Nat), p x := by
 intro u
 exists 0
 apply u

axiom Stab : ∀ (A : Prop), ¬¬A -> A

theorem LEM : A ∨ ¬ A := by
 apply Stab
 intro u
 apply u
 right
 intro v
 apply u
 left
 exact v
