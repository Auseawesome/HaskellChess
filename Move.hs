module Move() where
import Piece
import Board

addVec :: Coord -> Coord -> Coord
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

subVec :: Coord -> Coord -> Coord
subVec (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

emptyRaycast :: Board -> Coord -> Coord -> Coord -> Bool
emptyRaycast b v acc t | acc == t            = True
                       | not $ isEmpty b acc = False
                       | otherwise           = emptyRaycast b v (addVec acc v) t

isValidPawnDouble :: Board -> Coord -> Coord -> Bool
isValidPawnDouble b p1 p2 | y1 == y2          = False
                          | abs (y2 - y1) > 2 = False
                          | x1 /= x2          = False
                          | otherwise         = isEmpty b p2
                          where (x1, y1) = p1
                                (x2, y2) = p2

-- En passant is not checked for here, that should be a special case in move code
isValidPawnMove :: Board -> Color -> Coord -> Coord -> Bool
isValidPawnMove b c p1 p2 | c == White && y2 <= y1 = False                     -- White pawns can only move up in the y-direction
                          | c == Black && y2 >= y1 = False                     -- Black pawns can only move down in the y-direction
                          | abs (y2 - y1) /= 1     = isValidPawnDouble b p1 p2 -- Check double move if not moving 1 tile in y-direction
                          | x1 == x2               = isEmpty b p2              -- If moving directly forward, the tile must be empty
                          | abs(x1 - x2) /= 1      = False                     -- If not moving directly forward, only move 1 tile in x-direction
                          | otherwise              = isOpposing b c p2         -- If going diagonal, piece must be of opposing color
                          where (x1, y1) = p1
                                (x2, y2) = p2

isValidBishopMove :: Board -> Coord -> Coord -> Bool
isValidBishopMove b p1 p2 | (abs dx) /= (abs dy) = False
                          | otherwise            = emptyRaycast b v (addVec p1 v) p2
                          where (x1, y1) = p1
                                (x2, y2) = p2
                                dx = x2 - x1
                                dy = y2 - y1
                                v = ((signum dx), (signum dy))

isValidKnightMove :: Coord -> Coord -> Bool
isValidKnightMove p1 p2 | dx == 0                  = False
                        | dy == 0                  = False
                        | (abs dx) + (abs dy) == 3 = True
                        | otherwise                = False
                        where (dx, dy) = subVec p2 p1

isValidRookMove :: Board -> Coord -> Coord -> Bool
isValidRookMove b p1 p2 | dx == 0 || dy == 0 = emptyRaycast b v (addVec p1 v) p2
                        | otherwise          = False
                        where (dx, dy) = subVec p2 p1
                              v = ((signum dx), (signum dy))