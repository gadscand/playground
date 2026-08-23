-- Exercises 1.1.2
#eval 42 + 19 -- 19
#eval String.append "A" (String.append "B" "C") -- ABC
#eval String.append (String.append "A" "B") "C" -- ABC
#eval if 3 == 3 then 5 else 7 -- 5
#eval if 3 == 4 then "equal" else "not equal" -- "not equal"

-- Exercises 1.3.1.1
def joinStringWith (first : String) (second : String) (third : String) : String :=
    String.append second (String.append first third)
#eval joinStringWith ", " "one" "and another"
#check joinStringWith -- joinStringWith (first second third : String) : String
def volume (height : Nat) (width : Nat) (depth : Nat) : Nat :=
    height * width * depth
#eval volume 2 2 2

-- Exercises 1.4.3
structure RectangularPrism where
    height : Float
    width : Float
    depth : Float
def volumePrism (prism : RectangularPrism) : Float :=
    prism.height * prism.width * prism.depth
structure Point where
    x : Float -- x
    y : Float -- y
structure Segment where
    s : Point -- start point
    e : Point -- end point
def length (segment : Segment) : Float :=
    Float.sqrt
    (
        (segment.e.x - segment.s.x) ^ 2 +
        (segment.e.y - segment.s.y) ^ 2
    )
def a : Point :=
    { x := 0.0, y := 1.0 }
def b : Point :=
    { x := 2.0, y := 3.0 }
def segmentAB : Segment :=
    { s := a, e := b }
#eval length segmentAB


-- Others
def IsGreater (a b : Nat) : Bool :=
    if a ≥ b then
        Bool.true
    else
        Bool.false
#eval IsGreater 2 3
-- Read: Is 2 bigger than 3 ?
