module Maybe where
  data Maybe (A : Set) : Set where
    nothing : Maybe A
    just    : A → Maybe A 

  maybe : {A B : Set} → B → (A → B) → Maybe A → B
  maybe z f nothing = z
  maybe z f (just x) = f x