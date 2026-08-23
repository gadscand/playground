def f (a : Int) := a
def g (a: Int) := a + 1
def gf (a : Int): Int := g (f a)

#eval id f 1
#eval gf 1

#eval if gf (gf 1) == gf 1 then "true" else "false" -- Errado

-- Exercicios página 10, trabalhar nesse tema (Desafios)

-- Página 17 / 35
