module Vector(
    Vector(Vector, xPos, yPos),
    Coord,

    inBounds,

    divVec, modVec, modVecInt
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

instance Show a => Show (Vector a) where
    show (Vector x y)= "v(" ++ shows x (", " ++ shows y ")")

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

-- Special mod and div functions as a vector cannot be an integral
divVec :: Integral a => Vector a -> Vector a -> Vector a
divVec = liftA2 div

modVec :: Integral a => Vector a -> Vector a -> Vector a
modVec = liftA2 mod

modVecInt :: Integral a => Vector a -> Vector a -> a
modVecInt (Vector _ y1) (Vector 0 y2) = mod y1 y2
modVecInt (Vector x1 _) (Vector x2 0) = mod x1 x2
modVecInt v1            v2            = min (xPos vecMod) (yPos vecMod)
                                      where vecMod = modVec v1 v2