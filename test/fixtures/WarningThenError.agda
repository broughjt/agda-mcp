module WarningThenError where

open import Data.Nat using (ℕ; zero; suc)

first : ℕ → ℕ
first zero = zero
first (suc n) = n
first n = n

foo : ℕ → ℕ
foo n = ℕ
