module Unreachable where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

f : Nat → Nat
f x = zero
f zero = zero
