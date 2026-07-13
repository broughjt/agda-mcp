{-# OPTIONS --erasure #-}

module ContextAttributes where

data Nat : Set where
  zero : Nat

withAttributes : (@0 erased : Nat) → {{instanceArg : Nat}} → Nat
withAttributes erased {{instanceArg}} = {!!}
