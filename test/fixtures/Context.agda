module Context where

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

plus : Nat → Nat → Nat
plus zero y = {!!}
plus (suc x) y = {!!}

letBound : Nat
letBound =
  let one : Nat
      one = suc zero
   in {!!}
