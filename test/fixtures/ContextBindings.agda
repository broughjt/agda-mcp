module ContextBindings where

open import Data.Nat using (ℕ; _+_)

data P : ℕ → Set where

Twice : ℕ → Set
Twice n = P (n + n)

-- A local binder, a let binding, then another local binder, so that telescope
-- order and "locals before let bindings" disagree.
mixed : ℕ → ℕ → ℕ
mixed m = let doubled = m + m in λ n → ?

-- The inner binder shadows the outer one.
shadowed : ℕ → ℕ → ℕ
shadowed x = λ x → ?

normalized : Twice 2 → ℕ
normalized twice = ?

-- An anonymous binder has no name of its own, so Agda generates one.
anonymous : ℕ → ℕ → ℕ
anonymous _ n = ?
