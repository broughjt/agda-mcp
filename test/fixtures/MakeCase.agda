module MakeCase where

open import Data.Bool using (Bool; false; true)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.List using (List; []; _∷_)

-- Splitting on a variable.
double : ℕ → ℕ
double n = ?

-- A right-hand side with two holes, reused whole for every generated clause.
twoHoles : ℕ → ℕ
twoHoles n = ? + ?

-- A clause carrying a `where` block.
withWhere : ℕ → ℕ
withWhere n = ?
  where
    helper : ℕ
    helper = zero

-- Introducing an argument.
introduce : ℕ → ℕ
introduce = ?

-- Revealing a hidden argument rather than splitting on it.
hidden : {n : ℕ} → ℕ
hidden = ?

-- Expanding an ellipsis for a with-clause written with `...`.
filter : (ℕ → Bool) → List ℕ → List ℕ
filter p [] = []
filter p (x ∷ xs) with p x
... | true = x ∷ filter p xs
... | false = ?

-- An extended lambda.
extended : ℕ → ℕ
extended = λ { n → ? }

-- A goal in a type signature, which is not the right-hand side of a clause.
notAClause : ?
notAClause = zero
