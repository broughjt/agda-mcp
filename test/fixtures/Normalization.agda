module Normalization where

open import Data.Nat using (ℕ; _+_)

data P : ℕ → Set where

Twice : ℕ → Set
Twice n = P (n + n)

visible : Twice 2
visible = {!!}

hidden : Twice 2
hidden = _
