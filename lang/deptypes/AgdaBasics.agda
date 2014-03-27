module AgdaBasics where

id : {A : Set} → A → A
id x = x

identity : (A : Set) → A → A
identity A x = x

apply : (A : Set)(B : A → Set) → ((x : A) → B x) → (a : A) → B a
apply A B f a = f a

_∘_ : {A : Set}{B : A → Set}{C : (x : A) → B x → Set}
      (f : {x : A}(y : B x) → C x y)(g : (x : A) → B x)
      (x : A) → C x (g x)
(f ∘ g) x = f (g x)


data Nat : Set where
     zero : Nat
     suc  : Nat → Nat

_+_ : Nat → Nat → Nat
zero  + m = m
suc n + m = suc (n + m)

_*_ : Nat → Nat → Nat
zero  * m = zero
suc n * m = m + n * m

one : Nat
one = identity _ (suc zero)

plus-two = suc ∘ suc


data Bool : Set where
   true : Bool
   false : Bool

if_then_else : {A : Set} → Bool → A → A → A
if true then x else y = x
if false then x else y = y

not : Bool → Bool
not true = false
not false = true

infixl 60 _*_
infixl 40 _+_
-- infixr 20 _or_
infix  5 if_then_else



infixr 40 _::_
data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

map : {A B : Set} → (A → B) → List A → List B
map f []        = []
map f (x :: xs) = f x :: map f xs

_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

data Vec (A : Set) : Nat → Set where
  []   : Vec A zero
  _::_ : {n : Nat} → A → Vec A n → Vec A (suc n)

head : {A : Set}{n : Nat} → Vec A (suc n) → A
head (x :: xs) = x

vmap : {A B : Set}{n : Nat} → (A → B) → Vec A n → Vec B n
vmap f []        = []
vmap f (x :: xs) = f x :: vmap f xs

data Image_∋_ {A B : Set}(f : A → B) : B → Set where
  im : (x : A) → Image f ∋ f x

inv : {A B : Set}(f : A → B)(y : B) → Image f ∋ y → A
inv f .(f x) (im x) = x


data Fin : Nat → Set where
  fzero : {n : Nat} → Fin (suc n)
  fsuc  : {n : Nat} → Fin n → Fin (suc n)

_!_ : {n : Nat}{A : Set} → Vec A n → Fin n → A
[]        ! ()
(x :: xs) ! fzero    = x
(x :: xs) ! (fsuc i) = xs ! i

tabulate : {n : Nat}{A : Set} → (Fin n → A) → Vec A n
tabulate {zero}  f  = []
tabulate {suc n} f = f fzero :: tabulate (f ∘ fsuc)

data   False : Set where
record True  : Set where

trivial : True
trivial = _

isTrue : Bool → Set
isTrue true = True
isTrue false = False

_<_ : Nat → Nat → Bool
_     < zero  = false
zero  < suc n = true
suc m < suc n = m < n

length : {A : Set} → List A → Nat
length []        = zero
length (x :: xs) = suc (length xs)

lookup : {A : Set}(xs : List A)(n : Nat) → isTrue (n < length xs) → A
lookup []        n         ()
lookup (x :: xs) zero    p = x
lookup (x :: xs) (suc n) p = lookup xs n p


data _==_ {A : Set}(x : A) : A → Set where
  refl : x == x

data _≤_ : Nat → Nat → Set where
  leq-zero : {n : Nat} → zero ≤ n
  leq-suc  : {m n : Nat} → m ≤ n → suc m ≤ suc n
  
min : Nat → Nat → Nat
min x y with x < y
min x y | true = x
min x y | false = y

filter : {A : Set} → (A → Bool) → List A → List A
filter p [] = []
filter p (x :: xs) with p x
... | true  = x :: filter p xs
... | false = filter p xs

data _≠_ : Nat → Nat → Set where
  z≠s : {n : Nat} → zero ≠ suc n
  s≠z : {n : Nat} → suc n ≠ zero
  s≠s : {m n : Nat} → m ≠ n → suc m ≠ suc n

infix 20 _⊆_
data _⊆_ {A : Set} : List A → List A → Set where
  stop : [] ⊆ []
  drop : ∀ {xs y ys} → xs ⊆ ys → xs ⊆ y :: ys
  keep : ∀ {x xs ys} → xs ⊆ ys → x :: xs ⊆ x :: ys

{-
   Proof of the fact that filter result is a subset of its argument
-}
lem-filter : {A : Set}(p : A → Bool)(xs : List A) → 
             filter p xs ⊆ xs
lem-filter p [] = stop
lem-filter p (x :: xs) with p x
... | true  = keep (lem-filter p xs)
... | false = drop (lem-filter p xs)

