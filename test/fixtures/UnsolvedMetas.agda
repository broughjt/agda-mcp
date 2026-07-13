{-# OPTIONS --allow-unsolved-metas #-}
module UnsolvedMetas where

data Nat : Set where
  zero : Nat

postulate f : {A : Set} → A → Nat

test : Nat
test = f _
