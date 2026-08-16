module SlowCheck where

open import Data.Nat using (ℕ; zero)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

{-# TERMINATING #-}
spin : ℕ → ℕ
spin n = spin n

slow : spin zero ≡ zero
slow = refl
