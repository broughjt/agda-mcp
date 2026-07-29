module InferCheck where

open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Unit using (⊤; tt)

data P : ℕ → Set where

Twice : ℕ → Set
Twice n = P (n + n)

postulate twiceTwo : Twice 2

twice : ℕ → ℕ
twice n = n + n

hole : ℕ
hole = ?
