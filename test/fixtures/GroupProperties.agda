-- Excerpted from the standard library's `Algebra.Properties.Group` but trimmed
-- to four definitions.

{-# OPTIONS --without-K --safe #-}

open import Algebra.Bundles

module GroupProperties {g₁ g₂} (G : Group g₁ g₂) where

open Group G
open import Algebra.Definitions _≈_
open import Relation.Binary.Reasoning.Setoid setoid

//-cong₂ : Congruent₂ _//_
//-cong₂ = {!!}

//-rightDividesˡ : RightDividesˡ _∙_ _//_
//-rightDividesˡ x y = begin
  (y // x) ∙ x    ≈⟨ assoc y (x ⁻¹) x ⟩
  y ∙ (x ⁻¹ ∙ x)  ≈⟨ ∙-congˡ (inverseˡ x) ⟩
  y ∙ ε           ≈⟨ identityʳ y ⟩
  y               ∎

//-rightDividesʳ : RightDividesʳ _∙_ _//_
//-rightDividesʳ x y = begin
  y ∙ x // x    ≈⟨ assoc y x (x ⁻¹) ⟩
  y ∙ (x // x)  ≈⟨ ∙-congˡ (inverseʳ x) ⟩
  y ∙ ε         ≈⟨ {!!} ⟩
  y             ∎

ε⁻¹≈ε : ε ⁻¹ ≈ ε
ε⁻¹≈ε = {!!}
