def gcd : Nat → Nat → Nat
| 0, b => b
| a, 0 => a
| a + 1, b + 1 =>
  if a + 1 = b + 1 then a + 1
  else if a + 1 > b + 1 then gcd (a + 1 - (b + 1)) (b + 1)
  else gcd (a + 1) (b + 1 - (a + 1))
termination_by a b => a + b
decreasing_by all_goals omega
