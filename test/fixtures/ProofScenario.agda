module ProofScenario where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

+-assoc : (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p = {!!}
+-assoc (suc m) n p = {!!}
