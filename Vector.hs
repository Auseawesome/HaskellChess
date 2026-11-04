module Vector(
    Coord,

    addVec, subVec
) where

type Coord = (Int, Int)

liftVec1 :: (Int -> Int) -> Coord -> Coord
liftVec1 f (x, y) = (f x, f y)

liftVec2 :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
liftVec2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

addVec :: Coord -> Coord -> Coord
addVec = liftVec2 (+)

subVec :: Coord -> Coord -> Coord
subVec = liftVec2 (-)