module Move(isValidMove, makeMove, isInCheck) where
import ChessUtils
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
isValidPawnDouble b p1 p2 | y1 == y2           = False
                          | abs (y2 - y1) > 2  = False
                          | x1 /= x2           = False
                          | y1 /= 1 && y1 /= 6 = False
                          | otherwise          = isEmpty b p2
                          where (x1, y1) = p1
                                (x2, y2) = p2

isValidEnPassant :: Board -> Color -> Coord -> Coord -> Bool
isValidEnPassant b c p1 p2 | not (hasPawnDoubled b op p2) = False
                           | not (isEmpty b p2)           = False
                           | otherwise                    = isOpposing b c (x2, y1)
                           where (_, y1) = p1
                                 (x2, _) = p2
                                 op      = switchColor c

isValidPawnMove :: Board -> Color -> Coord -> Coord -> Bool
isValidPawnMove b c p1 p2 | c == White && y2 <= y1 = False                     -- White pawns can only move up in the y-direction
                          | c == Black && y2 >= y1 = False                     -- Black pawns can only move down in the y-direction
                          | abs (y2 - y1) /= 1     = isValidPawnDouble b p1 p2 -- Check double move if not moving 1 tile in y-direction
                          | x1 == x2               = isEmpty b p2              -- If moving directly forward, the tile must be empty
                          | abs(x1 - x2) /= 1      = False                     -- If not moving directly forward, only move 1 tile in x-direction
                          | isEnPassant            = True                      -- Handle en-passant special case
                          | otherwise              = isOpposing b c p2         -- If going diagonal, piece must be of opposing color
                          where (x1, y1)    = p1
                                (x2, y2)    = p2
                                isEnPassant = isValidEnPassant b c p1 p2

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

isValidQueenMove :: Board -> Coord -> Coord -> Bool
isValidQueenMove b p1 p2 = isValidBishopMove b p1 p2 || isValidRookMove b p1 p2

-- TODO: Stop castling through check
isValidCastle :: Board -> Color -> Coord -> Coord -> Bool
isValidCastle b c p1 p2 | dy /= 0                   = False
                        | (abs dx) /= 2             = False
                        | hasKingMoved b c          = False
                        | hasRookMoved b c rookSide = False
                        | not empty                 = False
                        | otherwise                 = Just (Piece c Rook) == (getPiece b rookPos)
                        where (dx, dy) = subVec p2 p1
                              (0 , y ) = p1
                              rookSide = if' (dx < 0) 0 8
                              rookPos  = (rookSide, y)
                              v        = (signum dx, 0)
                              empty    = emptyRaycast b v (addVec p1 v) rookPos

isValidKingMove :: Board -> Color -> Coord -> Coord -> Bool
isValidKingMove b c p1 p2 | validCastle  = True
                          | (abs dx) > 1 = False
                          | (abs dy) > 1 = False
                          | otherwise    = True
                          where (dx, dy)    = subVec p2 p1
                                validCastle = isValidCastle b c p1 p2

isValidMove :: Board -> Color -> Coord -> Coord -> Bool
isValidMove b c1 p1 p2 | not (onBoard p1) = False -- Can't move from outside of the board
                       | not (onBoard p2) = False -- Can't move out of the board
                       | empty            = False -- Can't move an empty tile
                       | c1 /= c2         = False -- Can't move a piece of opposing colour
                       | isAlly b c1 p2   = False -- Can't take piece of your own colour
                       | otherwise        = case pieceType of
                             Pawn   -> isValidPawnMove   b c1 p1 p2
                             Bishop -> isValidBishopMove b    p1 p2
                             Knight -> isValidKnightMove      p1 p2
                             Rook   -> isValidRookMove   b    p1 p2
                             Queen  -> isValidQueenMove  b    p1 p2
                             King   -> isValidKingMove   b c1 p1 p2
                       where piece                     = getPiece b p1
                             empty                     = piece == Nothing
                             Just (Piece c2 pieceType) = piece

updateKingFlags :: Color -> PieceType -> Board -> Board
updateKingFlags c King = flagKingMoved c
updateKingFlags _ _    = id

updateRookFlags :: Color -> PieceType -> Coord -> Board -> Board
updateRookFlags c Rook (x, _) = flagRookMoved c x
updateRookFlags _ _    _      = id

updatePawnFlags :: Color -> PieceType -> Coord -> Coord -> Board -> Board
updatePawnFlags c Pawn (x, y1) (_, y2) | abs (y2-y1) < 2 = clearPawnDoubled
                                       | otherwise       = flagPawnDoubled c x 
updatePawnFlags _ _    _       _                         = clearPawnDoubled

updateAllFlags :: Color -> PieceType -> Coord -> Coord -> Board -> Board
updateAllFlags c t p1 p2 b = b3
                           where b1 = updateKingFlags c t       b
                                 b2 = updateRookFlags c t p1    b1
                                 b3 = updatePawnFlags c t p1 p2 b2

enPassant :: Coord -> Coord -> Board -> Board
enPassant p1 p2 b = setPiece p1       Nothing         $
                    setPiece p2       (getPiece b p1) $
                    setPiece (x2, y1) Nothing         b
                  where (_, y1) = p1
                        (x2, _) = p2

castle :: Color -> Coord -> Coord -> Board -> Board
castle c p1 p2 b = movePiece (rookSide, y1) rookPos $
                   movePiece p1             p2      $
                   flagRookMoved c rookSide         b
                 where (dx, dy) = subVec p2 p1
                       (_ , y1) = p1
                       (x2, _ ) = p2
                       rookSide = if' (dx < 0) 0 8
                       rookPos  = (x2 - (signum dx), y1)

isInCheck :: Board -> Color -> Coord -> Bool
isInCheck b c p2 = any (\p1 -> isValidMove b (switchColor c) p1 p2) makeCoords

makeMove :: Color -> Coord -> Coord -> Board -> Board
makeMove c p1 p2 b | not valid   = b
                   | isEnPassant = updateAllFlags c t p1 p2 $ enPassant p1 p2 b
                   | isCastle    = updateAllFlags c t p1 p2 $ castle  c p1 p2 b
                   | otherwise   = updateAllFlags c t p1 p2 $ movePiece p1 p2 b
                   where valid            = isValidMove b c p1 p2
                         Just (Piece _ t) = getPiece b p1
                         isEnPassant      = (t == Pawn) && (isValidEnPassant b c p1 p2)
                         isCastle         = (t == King) && (isValidCastle    b c p1 p2)