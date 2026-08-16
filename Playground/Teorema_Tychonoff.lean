import Mathlib

example : IsCompact (Set.Icc (0 : ℝ) 1 ×ˢ Set.Icc (0 : ℝ) 1) := by
  exact IsCompact.prod isCompact_Icc isCompact_Icc
-- Esse exemplo é uma demonstração de que o quadrado fechado [0,1]^2 é compacto.

-- Teorema de Tychonoff
-- Seja X_i, i ∈ I uma familia de espaços compactos.
-- Então seu produto X = Π_{i∈I} X_i é compacto com respeito
-- a topologia produto. (Ou seja, a topologia com respeito as projeções
-- p_i: X → X_i ).

variable {ι : Type*} (X : ι → Type*) [∀ i, TopologicalSpace (X i)]
