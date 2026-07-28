{-# OPTIONS --erasure #-}

-- Binder attributes are a language feature rather than a library one, so this
-- fixture defines its own class to have something to take as an instance.
module ContextAttributes where

open import Data.Bool.Base using (Bool)
open import Data.Nat.Base using (ℕ)

record Equality (A : Set) : Set where
  field equal : A → A → Bool

instanceArgument : {{eq : Equality ℕ}} → ℕ → Bool
instanceArgument n = ?

erasedArgument : (@0 n : ℕ) → ℕ
erasedArgument n = ?

irrelevantArgument : .(n : ℕ) → ℕ
irrelevantArgument n = ?
