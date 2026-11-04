module Move(
      isValidMove,
      makeMove,

      isSpaceAttacked,
      isInCheck, currentlyInCheck
) where
import ChessUtils
import Piece
import Vector
import Board

emptyRaycast :: Board -> Coord -> Coord -> Coord -> Bool
emptyRaycast b v acc t | acc == t            = True
                       | not $ isEmpty b acc = False
                       | otherwise           = emptyRaycast b v (acc + v) t

isValidPawnDouble :: Board -> Coord -> Coord -> Bool
isValidPawnDouble b p1 p2 | y1 == y2           = False
                          | abs (y2 - y1) > 2  = False
                          | x1 /= x2           = False
                          | y1 /= 1 && y1 /= 6 = False
                          | otherwise          = isEmpty b p2
                          where (Vector x1 y1) = p1
                                (Vector x2 y2) = p2

isValidEnPassant :: Board -> Color -> Coord -> Coord -> Bool
isValidEnPassant b c p1 p2 | not (hasPawnDoubled b op p2) = False
                           | not (isEmpty b p2)           = False
                           | otherwise                    = isOpposing b c $ Vector x2 y1
                           where (Vector _ y1) = p1
                                 (Vector x2 _) = p2
                                 op            = switchColor c

isValidPawnMove :: Board -> Coord -> Coord -> Bool
isValidPawnMove b p1 p2 | c == White && y2 <= y1 = False                     -- White pawns can only move up in the y-direction
                        | c == Black && y2 >= y1 = False                     -- Black pawns can only move down in the y-direction
                        | abs (y2 - y1) /= 1     = isValidPawnDouble b p1 p2 -- Check double move if not moving 1 tile in y-direction
                        | x1 == x2               = isEmpty b p2              -- If moving directly forward, the tile must be empty
                        | abs(x1 - x2) /= 1      = False                     -- If not moving directly forward, only move 1 tile in x-direction
                        | isEnPassant            = True                      -- Handle en-passant special case
                        | otherwise              = isOpposing b c p2         -- If going diagonal, piece must be of opposing color
                        where (Vector x1 y1)   = p1
                              (Vector x2 y2)   = p2
                              Just (Piece c _) = getPiece b p1
                              isEnPassant      = isValidEnPassant b c p1 p2

isValidBishopMove :: Board -> Coord -> Coord -> Bool
isValidBishopMove b p1 p2 | (abs dx) /= (abs dy) = False
                          | otherwise            = emptyRaycast b v (p1 + v) p2
                          where (Vector dx dy) = p2 - p1
                                v              = Vector (signum dx) (signum dy)

isValidKnightMove :: Coord -> Coord -> Bool
isValidKnightMove p1 p2 | dx == 0                  = False
                        | dy == 0                  = False
                        | (abs dx) + (abs dy) == 3 = True
                        | otherwise                = False
                        where (Vector dx dy) = p2 - p1

isValidRookMove :: Board -> Coord -> Coord -> Bool
isValidRookMove b p1 p2 | dx == 0 || dy == 0 = emptyRaycast b v (p1 + v) p2
                        | otherwise          = False
                        where (Vector dx dy) = p2 - p1
                              v              = (Vector (signum dx) (signum dy))

isValidQueenMove :: Board -> Coord -> Coord -> Bool
isValidQueenMove b p1 p2 = isValidBishopMove b p1 p2 || isValidRookMove b p1 p2

-- TODO: Stop castling through check
isValidCastle :: Board -> Coord -> Coord -> Bool
isValidCastle b p1 p2 | dy /= 0                   = False
                      | (abs dx) /= 2             = False
                      | hasKingMoved b c          = False
                      | hasRookMoved b c rookSide = False
                      | not empty                 = False
                      | otherwise                 = Just (Piece c Rook) == (getPiece b rookPos)
                      where Vector dx dy     = p2 - p1
                            Just (Piece c _) = getPiece b p1
                            rookSide         = if' (dx < 0) 0 8
                            rookPos          = p1 {xPos = rookSide}
                            v                = Vector (signum dx) 0
                            empty            = emptyRaycast b v (p1 + v) rookPos

isValidKingMove :: Board -> Coord -> Coord -> Bool
isValidKingMove b p1 p2 | validCastle = True
                        | abs dx > 1  = False
                        | abs dy > 1  = False
                        | otherwise   = True
                        where Vector dx dy = p2 - p1
                              validCastle  = isValidCastle b p1 p2

isValidMove :: Board -> Color -> Coord -> Coord -> Bool
isValidMove b c1 p1 p2 | not (onBoard p1) = False -- Can't move from outside of the board
                       | not (onBoard p2) = False -- Can't move out of the board
                       | p1 == p2         = False -- Can't move to the same tile that you start on
                       | empty            = False -- Can't move an empty tile
                       | c1 /= c2         = False -- Can't move a piece of opposing colour
                       | isAlly b c1 p2   = False -- Can't take piece of your own colour
                       | otherwise        = case pieceType of
                             Pawn   -> isValidPawnMove   b p1 p2
                             Bishop -> isValidBishopMove b p1 p2
                             Knight -> isValidKnightMove   p1 p2
                             Rook   -> isValidRookMove   b p1 p2
                             Queen  -> isValidQueenMove  b p1 p2
                             King   -> isValidKingMove   b p1 p2
                       where piece                     = getPiece b p1
                             empty                     = piece == Nothing
                             Just (Piece c2 pieceType) = piece

updateKingFlags :: Color -> PieceType -> Board -> Board
updateKingFlags c King = flagKingMoved c
updateKingFlags _ _    = id

updateRookFlags :: Color -> PieceType -> Coord -> Board -> Board
updateRookFlags c Rook p = flagRookMoved c $ xPos p
updateRookFlags _ _    _ = id

updatePawnFlags :: Color -> PieceType -> Coord -> Coord -> Board -> Board
updatePawnFlags c Pawn p1 p2 | abs dy < 2 = clearPawnDoubled
                             | otherwise  = flagPawnDoubled c x
                             where dy = yPos (p2 - p1)
                                   x  = xPos p1
updatePawnFlags _ _    _  _               = clearPawnDoubled

updateAllFlags :: Color -> PieceType -> Coord -> Coord -> Board -> Board
updateAllFlags c t p1 p2 b = updatePawnFlags c t p1 p2 $
                             updateRookFlags c t p1    $
                             updateKingFlags c t       b

enPassant :: Coord -> Coord -> Board -> Board
enPassant p1 p2 b = setPiece p1     Nothing         $
                    setPiece p2     (getPiece b p1) $
                    setPiece pSpace Nothing         b
                  where pSpace = Vector (xPos p2) (yPos p1)

castle :: Color -> Coord -> Coord -> Board -> Board
castle c p1 p2 b = movePiece rookOrigin rookPos $
                   movePiece p1         p2      $
                   flagRookMoved c rookSide     b
                 where Vector dx dy = p2 - p1
                       x            = xPos p2
                       rookSide     = if' (dx < 0) 0 8
                       rookOrigin   = p1 {xPos = rookSide}
                       rookPos      = p1 {xPos = x - signum dx}

isSpaceAttacked :: Board -> Color -> Coord -> Bool
isSpaceAttacked b c p2 = any (\p1 -> isValidMove b (switchColor c) p1 p2) makeCoords

isInCheck :: Color -> Board -> Bool
isInCheck c b = isSpaceAttacked b c $ findKing b c

currentlyInCheck :: Board -> Bool
currentlyInCheck b = isInCheck (turn b) b

makeMove :: Color -> Coord -> Coord -> Board -> Board
makeMove c p1 p2 b | not valid   = b
                   | isEnPassant = uaf $ enPassant p1 p2 b
                   | isCastle    = uaf $ castle  c p1 p2 b
                   | otherwise   = uaf $ movePiece p1 p2 b
                   where valid            = isValidMove b c p1 p2
                         Just (Piece _ t) = getPiece b p1
                         isEnPassant      = (t == Pawn) && (isValidEnPassant b c p1 p2)
                         isCastle         = (t == King) && (isValidCastle    b   p1 p2)
                         uaf              = updateAllFlags c t p1 p2