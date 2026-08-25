-- Identidade
def f (a : Int) := a + 1
def my_id (a : Int) := a - 1
#eval my_id (f 1) == 1 -- Verifica se my_id funciona
-- Composição
def g (a : Int) := a * 2
def g ∘ f := g f a
#eval g ∘ f 1

-- Desafios págia 43 / 25

-- Sim, a WWW é uma categoria que relaciona páginas (objetos) a outras páginas (hyperlinks)
-- Sim, hyperlinks são morfismos

-- Sim, um morfismo de uma pessoa nela mesma séria a identidade e as relações de amizades
-- levam uma pessoa a um amigo em particular.

-- Pesquisar "Quando directed graph é uma categoria"
