module Vector(
    Coord,

    liftVec1, liftVec2,

    divVec, modVec
) where

type Vector a = (a, a)

type Coord = Vector Int

liftVec1 :: (a -> a) -> Vector a -> Vector a
liftVec1 f (x, y) = (f x, f y)

liftVec2 :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
liftVec2 f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

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