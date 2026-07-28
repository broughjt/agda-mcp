module Parenthesization where

open import Data.Nat using (ℕ)

apply : (ℕ → ℕ) → ℕ → ℕ
apply f n = f n

test : ℕ
test = apply {!!} 0
