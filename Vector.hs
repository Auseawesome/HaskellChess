module Vector(
    Coord,

    liftVec1, liftVec2,

    divVec, modVec
) where

type Coord = (Int, Int)

liftVec1 :: (Int -> Int) -> Coord -> Coord
liftVec1 f (x, y) = (f x, f y)

liftVec2 :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
liftVec2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

instance Num Coord where
    -- Binary Num Functions
    (+) = liftVec2 (+)
    (-) = liftVec2 (-)
    (*) = liftVec2 (*)

    -- Single Num Functions
    negate = liftVec1 negate
    abs    = liftVec1 negate
    signum = liftVec1 signum

    -- Special Case Function
    fromInteger x = (fromInteger x, fromInteger x)

-- Special mod and div functions and coord cannot be an integral
divVec :: Coord -> Coord -> Coord
divVec = liftVec2 div

modVec :: Coord -> Coord -> Coord
modVec = liftVec2 mod