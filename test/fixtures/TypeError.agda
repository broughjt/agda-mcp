module TypeError where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

bad : Nat
bad = suc
