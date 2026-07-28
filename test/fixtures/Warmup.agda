-- Nearly all of the time of a cold load fixture is deserializing the standard
-- library's files, so we load this once per test run to populate Agda's
-- decoded-module cache. The imports here should consist of the union of modules
-- needed in the other fixtures, and should be updated when new fixtures are
-- added or existing ones are modified.

module Warmup where

import Algebra.Definitions
import Relation.Binary.Reasoning.Setoid

open import Algebra.Bundles using (Group)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Unit using (⊤; tt)
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
