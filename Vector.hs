module Vector(
    Coord,

    toVec, zipVec,

    liftVec1, liftVec2,

    divVec, modVec
) where

type Vector a = (a, a)

type Coord = Vector Int

toVec :: a -> Vector a
toVec x = (x, x)

zipVec :: Vector (a -> b) -> Vector a -> Vector b
zipVec (fx, fy) (x, y) = (fx x, fy y)

liftVec1 :: (a -> b) -> Vector a -> Vector b
liftVec1 f = zipVec (toVec f)

liftVec2 :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
liftVec2 f p1 = zipVec (liftVec1 f p1)

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
    fromInteger x = (fromInteger x, fromInteger x)

-- Special mod and div functions and coord cannot be an integral
divVec :: Integral a => Vector a -> Vector a -> Vector a
divVec = liftVec2 div

modVec :: Integral a => Vector a -> Vector a -> Vector a
modVec = liftVec2 mod