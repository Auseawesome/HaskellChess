module Vector(
    Coord,

    addVec, subVec
) where

type Coord = (Int, Int)

addVec :: Coord -> Coord -> Coord
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVec :: Coord -> Coord -> Coord
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)