-- `*-distribʳ-+` from the Agda standard library
-- (Data/Nat/Properties.agda:805-812) with the proof erased.

module NatDistrib where

import Algebra.Definitions as AlgebraicDefinitions
open import Data.Nat.Base using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-assoc)
open import Relation.Binary.PropositionalEquality
  using (_≡_; refl; cong; sym; trans)

open AlgebraicDefinitions {A = ℕ} _≡_

*-distribʳ-+ : _*_ DistributesOverʳ _+_
*-distribʳ-+ = ?
