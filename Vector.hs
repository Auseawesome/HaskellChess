module Vector(
    Vector(Vector, xPos, yPos),
    Coord,

    inBounds,

    divVec, modVec
) where

data Vector a = Vector {xPos :: a, yPos :: a}
    deriving (Eq)

type Coord = Vector Int

-- Check whether a vector is within some bounds, lower inclusive bound and exclusive upper bound
inBounds :: Ord a => Vector a -> Vector a -> Vector a -> Bool
inBounds l h p = x >= lx && y >= ly && x < hx && y < hy
               where Vector lx ly = l
                     Vector hx hy = h
                     Vector x  y  = p

instance Functor Vector where
    fmap   = (<*>) . pure
    x <$ _ = pure x

instance Applicative Vector where
    pure x  = Vector x x
    
    fv <*> v = Vector (fx x) (fy y)
             where (Vector fx fy) = fv
                   (Vector x  y ) = v

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