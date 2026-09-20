import Mathlib.NumberTheory.LegendreSymbol.Basic
import Mathlib.NumberTheory.LegendreSymbol.QuadraticChar.GaussSum
namespace legendreSym

open Nat
open ZMod

/--
**The Law of Quadratic Reciprocity**:
if `p` and `q` are distinct odd primes, then
`(q / p) * (p / q) = (-1)^((p-1)(q-1)/4)`.
-/
theorem quadratic_reciprocity
    {p q : Nat} (hp : p ≠ 2)
    (hq : q ≠ 2) (hpq : p ≠ q)
    [Fact (Nat.Prime p)] [Fact (Nat.Prime q)] :
    legendreSym q p * legendreSym p q = (-1) ^ (p / 2 * (q / 2)) := by
  -- Nota: Prime.eq_two_or_odd, uma proposição que, um número primo é igual a 2 ou, é impar.
  -- Nota: Procurar sobre o funcionamento de @Fact.out
  -- Nota: Procurar sobre o resolve_left
  have hp₁ := (Prime.eq_two_or_odd <| @Fact.out p.Prime _).resolve_left hp
  have hq₁ := (Prime.eq_two_or_odd <| @Fact.out q.Prime _).resolve_left hq
  -- ringChar diz que: É uma função não computável que,
  -- a saída é a caracteristica única de um semi-anel
  have hq₂ : ringChar (ZMod q) ≠ 2 := (ringChar_zmod_n q).substr hq
  have h :=
    quadraticChar_odd_prime ((ringChar_zmod_n p).substr hp) hq ((ringChar_zmod_n p).substr hpq)
  rw [card p] at h
  have nc : ∀ n r : ℕ, ((n : ℤ) : ZMod r) = n := fun n r => by norm_cast
  have nc' : (((-1) ^ (p / 2) : ℤ) : ZMod q) = (-1) ^ (p / 2) := by norm_cast
  rw [legendreSym, legendreSym, nc, nc, h, map_mul, mul_rotate', mul_comm (p / 2), ← pow_two,
    quadraticChar_sq_one (prime_ne_zero q p hpq.symm), mul_one, pow_mul, χ₄_eq_neg_one_pow hp₁, nc',
    map_pow, quadraticChar_neg_one hq₂, card q, χ₄_eq_neg_one_pow hq₁]

-- Declara formalmente que 7 é um número primo para o ambiente de testes
instance : Fact (Nat.Prime 7) :=  ⟨by decide⟩

-- Agora o #eval vai funcionar perfeitamente!
#eval legendreSym 7 3

end legendreSym
