module Constraints where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

f : {!!}
f zero = zero
f (suc n) = n
