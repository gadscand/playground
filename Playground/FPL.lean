def IsGreater (a b : Nat) : Bool :=
    if a ≥ b then
        Bool.true
    else
        Bool.false
#eval IsGreater 2 3
-- Read: Is 2 bigger than 3 ?
