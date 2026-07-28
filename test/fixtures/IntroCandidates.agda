module IntroCandidates where

open import Data.Bool using (Bool; false; true)
open import Data.Empty using (⊥)
open import Data.Nat using (ℕ)

postulate Abstract : Set

noConstructor : ⊥
noConstructor = ?

noCandidate : Abstract
noCandidate = ?

ambiguous : Bool
ambiguous = ?

lambda : ℕ → ℕ
lambda = ?

patternLambda : ℕ → ℕ
patternLambda = ?
