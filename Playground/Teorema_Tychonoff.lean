import Mathlib

example : IsCompact (Set.Icc (0 : ℝ) 1 ×ˢ Set.Icc (0 : ℝ) 1) := by
  exact IsCompact.prod isCompact_Icc isCompact_Icc
-- Esse exemplo é uma demonstração de que o quadrado fechado [0,1]^2 é compacto.

-- Teorema de Tychonoff
-- Seja X_i, i ∈ I uma familia de espaços compactos.
-- Então seu produto X = Π_{i∈I} X_i é compacto com respeito
-- a topologia produto. (Ou seja, a topologia com respeito as projeções
-- p_i: X → X_i ).

variable {ι : Type*} (X : ι → Type*)
  [∀ i, TopologicalSpace (X i)]
  [∀ i, CompactSpace (X i)]
-- Cada conjunto X_i é um espaço topologico e é compacto.

-- Definindo o espaço produto
def ProductSpace := Π i, X i

-- As projeções
def proj (i : ι) : (Π j, X j) → X i := fun f => f i

-- Trabalhando com bases e sub bases.

-- A topologia gerada por S (menor topologia contendo S)
def generateFrom {X : Type*} (S : Set (Set X)) : TopologicalSpace X :=
  ⨆ (t : TopologicalSpace X) (_ : S ⊆ t.IsOpen), t

-- S é uma subbase para a topologia se a gera
def IsSubbasis {X : Type*} (S : Set (Set X)) [TopologicalSpace X] : Prop :=
  generateFrom S = ‹TopologicalSpace X›

-- Teorema de Alexander: X é compacto se, e somente se,
-- toda cobertura por elementos da subbase S tem subcobertura finita
theorem alexander_subbasis {X : Type*} [TopologicalSpace X] (S : Set (Set X))
    (hS : IsSubbasis S) :
    IsCompact (Set.univ : Set X) ↔
    ∀ (C : Set (Set X)), C ⊆ S → (Set.univ ⊆ ⋃₀ C) →
      ∃ D ⊆ C, D.Finite ∧ Set.univ ⊆ ⋃₀ D := by
  sorry

-- Define subbasis dos intervalos (-∞, a) e (b, ∞)
def leftInterInfty (a : ℝ) : Set ℝ := {x | x < a}
def rightInterInfty (b : ℝ) : Set ℝ := {x | b < x}

-- Subbase para a topologia usual de ℝ
def interSubbasis : Set (Set ℝ) :=
  {U | ∃ a : ℝ, U = leftInterInfty a ∨ U = rightInterInfty a}
