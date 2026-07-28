-- `++-assoc` as the standard library states and proves it
-- (Data/List/Properties.agda:138-140), with the two right-hand sides blanked
-- out. The bodies the tests give back are the library's own text.

module StandardLibraryProof where

import Algebra.Definitions as AlgebraicDefinitions
open import Data.List.Base using (List; []; _∷_; _++_)
open import Level using (Level)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)

private
  variable
    a : Level

module _ {A : Set a} where

  open AlgebraicDefinitions {A = List A} _≡_

  ++-assoc : Associative _++_
  ++-assoc []       ys zs = ?
  ++-assoc (x ∷ xs) ys zs = ?
