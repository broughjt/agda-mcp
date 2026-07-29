module Constrained where

open import Data.Nat using (ℕ; _+_)

postulate
  P : ℕ → Set
  p : (n : ℕ) → P (n + n)

-- Checking `p ?` against `P 4` leaves the equation `? + ? = 4`, which Agda
-- cannot solve and postpones as a constraint mentioning the hole.
constrained : P 4
constrained = p ?
