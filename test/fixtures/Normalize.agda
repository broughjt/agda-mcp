module Normalize where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

plus : Nat → Nat → Nat
plus zero y = y
plus (suc x) y = suc (plus x y)

double : Nat → Nat
double zero = zero
double (suc n) = suc (suc (double n))

data _≡_ (x : Nat) : Nat → Set where
  refl : x ≡ x

lemma : double zero ≡ plus zero zero
lemma = {!!}
