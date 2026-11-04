module Vector(
    Coord,

    toVec, vecApply,

    liftVec1, liftVec2,

    divVec, modVec
) where

type Vector a = (a, a)

type Coord = Vector Int

toVec :: a -> Vector a
toVec x = (x, x)

vecApply :: Vector (a -> b) -> Vector a -> Vector b
vecApply (fx, fy) (x, y) = (fx x, fy y)

liftVec1 :: (a -> b) -> Vector a -> Vector b
liftVec1 = vecApply . toVec

liftVec2 :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftVec2 = (vecApply .) . liftVec1

instance Num a => Num (Vector a) where
    -- Binary Num Functions
    (+) = liftVec2 (+)
    (-) = liftVec2 (-)
    (*) = liftVec2 (*)

    -- Single Num Functions
    negate = liftVec1 negate
    abs    = liftVec1 negate
    signum = liftVec1 signum

    -- Special Case Function
    fromInteger = toVec . fromInteger

-- Special mod and div functions and coord cannot be an integral
divVec :: Integral a => Vector a -> Vector a -> Vector a
divVec = liftVec2 div

modVec :: Integral a => Vector a -> Vector a -> Vector a
modVec = liftVec2 mod