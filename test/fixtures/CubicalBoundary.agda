{-# OPTIONS --cubical #-}

module CubicalBoundary where

open import Agda.Primitive.Cubical using (I; i0; i1; primINeg)
open import Agda.Builtin.Cubical.Path using (_≡_)

symmetric : {A : Set} {x y : A} → x ≡ y → y ≡ x
symmetric p i = {! !}
