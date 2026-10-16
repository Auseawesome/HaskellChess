module Move() where
import Piece
import Board

emptyRaycast :: Board -> Coord -> Coord -> Coord -> Bool
emptyRaycast b v acc t | acc == t            = True
                       | not $ isEmpty b acc = False
                       | otherwise           = emptyRaycast b v next t
                       where (vx, vy) = v
                             (accX, accY) = acc
                             next = (accX + vx, accY + vy)

isValidPawnDouble :: Board -> Coord -> Coord -> Bool
isValidPawnDouble b p1 p2 | abs (y2 - y1) > 2 = False
                          | x1 /= x2          = False
                          | otherwise         = isEmpty b p2
                          where (x1, y1) = p1
                                (x2, y2) = p2

-- En passant is not checked for here, that should be a special case in move code
isValidPawnMove :: Board -> Color -> Coord -> Coord -> Bool
isValidPawnMove b c p1 p2 | c == White && y2 <= y1 = False             -- White pawns can only move up in the y-direction
                          | c == Black && y2 >= y1 = False             -- Black pawns can only move down in the y-direction
                          | abs (y2 - y1) /= 1     = False             -- Check double move if moving more than 1 tile in y-direction
                          | x1 == x2               = isEmpty b p2      -- If moving directly forward, the tile must be empty
                          | abs(x1 - x2) /= 1      = False             -- If not moving directly forward, only move 1 tile in x-direction
                          | otherwise              = isOpposing b c p2 -- If going diagonal, piece must be of opposing color
                          where (x1, y1) = p1
                                (x2, y2) = p2