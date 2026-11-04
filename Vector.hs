module Vector(
    Vector(Vector, xPos, yPos),
    Coord,

    vecApply,

    divVec, modVec
) where

data Vector a = Vector {xPos :: a, yPos :: a}
    deriving (Eq)

type Coord = Vector Int

vecApply :: Vector (a -> b) -> Vector a -> Vector b
vecApply (Vector fx fy) (Vector x y) = Vector (fx x) (fy y)

instance Functor Vector where
    fmap   = vecApply . pure
    x <$ _ = pure x

instance Applicative Vector where
    pure x  = Vector x x
    liftA2 = (vecApply .) . fmap

instance Num a => Num (Vector a) where
    -- Binary Num Functions
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)

    -- Single Num Functions
    negate = fmap negate
    abs    = fmap negate
    signum = fmap signum

    -- Special Case Function
    fromInteger = pure . fromInteger

-- Special mod and div functions and coord cannot be an integral
divVec :: Integral a => Vector a -> Vector a -> Vector a
divVec = liftA2 div

modVec :: Integral a => Vector a -> Vector a -> Vector a
modVec = liftA2 mod