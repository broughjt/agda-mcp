module Warnings where

open import Data.Nat using (ℕ; zero; suc)

first : ℕ → ℕ
first zero = zero
first (suc n) = n
first n = n

second : ℕ → ℕ
second zero = zero
second (suc n) = n
second n = n
