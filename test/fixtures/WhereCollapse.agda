module WhereCollapse where

open import Data.Nat using (ℕ; zero; suc; _+_)

withWhere : ℕ → ℕ
withWhere n = ? + helper
  where
    helper : ℕ
    helper = zero
