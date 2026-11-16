module Move(
      validMoves, makeMove,

      isSpaceAttacked,
      isInCheck, currentlyInCheck,

      pieceCanMove, anyMovesLeft,

      moveCausesCheck
) where
import ChessUtils
import Piece
import Vector
import Board

type MovePredicate = Board -> Coord -> Coord -> Bool

type MoveLister = Board -> Coord -> [Coord]

emptyRaycast :: Board -> Coord -> Coord -> Coord -> Bool
emptyRaycast b v acc t | acc == t            = True
                       | not $ isEmpty b acc = False
                       | otherwise           = emptyRaycast b v (acc + v) t

emptyRaycastList :: Board -> Coord -> Coord -> [Coord]
emptyRaycastList b v acc | not $ onBoard acc   = []
                         | not $ isEmpty b acc = []
                         | otherwise           = acc : emptyRaycastList b v (acc + v)

emptyRaycastPiece :: Board -> Coord -> Coord -> Tile
emptyRaycastPiece b v acc | not $ onBoard acc   = Nothing
                          | not $ isEmpty b acc = getTile b acc
                          | otherwise           = emptyRaycastPiece b v (acc + v)


isValidPawnDouble :: MovePredicate
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
                           where y1 = yPos p1
                                 x2 = xPos p2
                                 op = switchColor c

getPawnDir :: Color -> Int
getPawnDir c = if' (c == White) 1 (-1)

validPawnSingle :: MoveLister
validPawnSingle b p = if' (em) [p'] []
                    where c  = colorAt b p
                          dp = Vector 0 (getPawnDir c)
                          p' = p + dp
                          em = Nothing == getTile b p'                     

validPawnDouble :: MoveLister
validPawnDouble b p = if' (em && or) [p'] []
                    where c  = colorAt b p
                          dp = Vector 0 (getPawnDir c * 2)
                          p' = p + dp
                          em = Nothing == getTile b p'
                          y  = yPos p
                          or = y == 1 || y == 6             

validPawnTakes :: MoveLister
validPawnTakes b p = do dx <- [-1, 1]
                        let c  = colorAt b p
                        let dp = Vector dx (getPawnDir c)
                        let p' = p + dp

                        true <- return (Nothing /= getTile b p' || isValidEnPassant b c p p')

                        return p'

validPawnMoves :: MoveLister
validPawnMoves b p = validPawnSingle b p ++ validPawnDouble b p ++ validPawnTakes b p

validBishopMoves :: MoveLister
validBishopMoves b p = do vx <- [-1, 1]
                          vy <- [-1, 1]
                          let v = Vector vx vy
                          emptyRaycastList b v (p + v)

validKnightMoves :: MoveLister
validKnightMoves b p = do ax <- [1 , 2]
                          mx <- [-1, 1]
                          my <- [-1 ,1]
                          let vx = ax       * mx
                          let vy = (3 - ax) * my

                          return $ p + Vector vx vy

validRookMoves :: MoveLister
validRookMoves b p = do s <- [-1, 1]
                        v <- [Vector 0 s, Vector s 0]
                        emptyRaycastList b v (p + v)

validQueenMoves :: MoveLister
validQueenMoves b p = validBishopMoves b p ++ validRookMoves b p

-- TODO: Stop castling through check
isValidCastle :: MovePredicate
isValidCastle b p1 p2 | dy /= 0                   = False
                      | (abs dx) /= 2             = False
                      | hasKingMoved b c          = False
                      | hasRookMoved b c rookSide = False
                      | not empty                 = False
                      | otherwise                 = Just (Piece c Rook) == (getTile b rookPos)
                      where Vector dx dy     = p2 - p1
                            Just (Piece c _) = getTile b p1
                            rookSide         = if' (dx < 0) 0 8
                            rookPos          = p1 {xPos = rookSide}
                            v                = Vector (signum dx) 0
                            empty            = emptyRaycast b v (p1 + v) rookPos

validNormalKingMoves :: MoveLister
validNormalKingMoves b p = do dx <- [-1, 0, 1]
                              dy <- [-1, 0, 1]

                              true <- return ((dx + dy) == 0)

                              let c  = colorAt b p
                              let p2 = p + Vector dx dy
                              false <- return $ isSpaceAttacked b c p2

                              return p2

validKingMoves :: MoveLister
validKingMoves b p = filter (isValidCastle b p) [l, r] ++ 
                     validNormalKingMoves  b p
                   where l = p {xPos = 2}
                         r = p {xPos = 6}

genericMoveValid :: Board -> Color -> Coord -> Coord -> Bool
genericMoveValid b c1 p1 p2 | not (onBoard p1) = False -- Can't move from outside of the board
                            | not (onBoard p2) = False -- Can't move out of the board
                            | p1 == p2         = False -- Can't move to the same tile that you start on
                            | empty            = False -- Can't move an empty tile
                            | tt == King       = False -- Can't take a King
                            | c1 /= c2         = False -- Can't move a piece of opposing colour
                            | isAlly b c1 p2   = False -- Can't take piece of your own colour
                            | cc               = False -- Can't do a move which causes check
                            | ic               = False -- If in check, can't make a move which doesn't resolve check
                            | otherwise        = True
                            where tile  = getTile b p1
                                  empty = tile == Nothing
                                  tt    = tileType tile
                                  c2    = tileColor tile
                                  cc    = moveCausesCheck b c1 p1 p2
                                  b'    = makeMove c1 p1 p2 b
                                  ic    = isInCheck c1 b && isInCheck c1 b'

validMoves :: MoveLister
validMoves b p | not (onBoard p) = []
               | empty           = []
               | otherwise       = filter (genericMoveValid b c p) $ ml b p
               where tile  = getTile b p
                     empty = tile == Nothing
                     c     = tileColor tile
                     ml    = case tileType tile of
                                  Pawn   -> validPawnMoves
                                  Bishop -> validBishopMoves
                                  Knight -> validKnightMoves
                                  Rook   -> validRookMoves
                                  Queen  -> validQueenMoves
                                  King   -> validKingMoves

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
enPassant p1 p2 b = setTile p1     Nothing        $
                    setTile p2     (getTile b p1) $
                    setTile pSpace Nothing        b
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

spaceAttackedByPawn :: Board -> Color -> Coord -> Bool
spaceAttackedByPawn b c p = any id checks
                          where checks = do dx <- [-1, 1]
                                            let dy = getPawnDir c
                                            let dp = Vector dx dy

                                            return $ (getTile b (p + dp) == Just (Piece (switchColor c) Pawn))

spaceAttackedByKing :: Board -> Color -> Coord -> Bool
spaceAttackedByKing b c p = any id checks
                          where checks = do dx <- [-1, 0, 1]
                                            dy <- [-1, 0, 1]
                                            let dp = Vector dx dy

                                            return $ (getTile b p == Just (Piece (switchColor c) King))

spaceAttackedByKnight :: Board -> Color -> Coord -> Bool
spaceAttackedByKnight b c p = any knightPred $ validKnightMoves b p
                            where knightPred = (== Just (Piece (switchColor c) Knight)).(getTile b)

spaceAttackedStraight :: Board -> Color -> Coord -> Bool
spaceAttackedStraight b c p = any id checks
                            where checks = do s <- [-1, 1]
                                              v <- [Vector 0 s, Vector s 0]
                                              return $ attackerPred $ emptyRaycastPiece b v (p + v)
                                  attackerPred :: Tile -> Bool
                                  attackerPred t | t == Nothing = False
                                                 | tc == c      = False
                                                 | tt == Queen  = True
                                                 | tt == Rook   = True
                                                 | otherwise    = False
                                                 where tt = tileType  t
                                                       tc = tileColor t

spaceAttackedDiagonally :: Board -> Color -> Coord -> Bool
spaceAttackedDiagonally b c p = any id checks
                              where checks = do vx <- [-1, 1]
                                                vy <- [-1, 1]
                                                let v = Vector vx vy
                                                return $ attackerPred $ emptyRaycastPiece b v (p + v)
                                    attackerPred :: Tile -> Bool
                                    attackerPred t | t == Nothing = False
                                                   | tc == c      = False
                                                   | tt == Queen  = True
                                                   | tt == Bishop = True
                                                   | otherwise    = False
                                                   where tt = tileType  t
                                                         tc = tileColor t

isSpaceAttacked :: Board -> Color -> Coord -> Bool
isSpaceAttacked b c p = spaceAttackedByPawn     b c p
                     || spaceAttackedByKing     b c p
                     || spaceAttackedByKnight   b c p
                     || spaceAttackedStraight   b c p
                     || spaceAttackedDiagonally b c p

isInCheck :: Color -> Board -> Bool
isInCheck c b = isSpaceAttacked b c $ findKing b c

currentlyInCheck :: Board -> Bool
currentlyInCheck b = isInCheck (turn b) b

makeMove :: Color -> Coord -> Coord -> Board -> Board
makeMove c p1 p2 b | isEnPassant = uaf $ enPassant p1 p2 b
                   | isCastle    = uaf $ castle  c p1 p2 b
                   | otherwise   = uaf $ movePiece p1 p2 b
                   where t           = typeAt b p1
                         isEnPassant = (t == Pawn) && (isValidEnPassant b c p1 p2)
                         isCastle    = (t == King) && (isValidCastle    b   p1 p2)
                         uaf         = updateAllFlags c t p1 p2

pieceCanMove :: Board -> Coord -> Bool
pieceCanMove b p = [] /= validMoves b p

anyMovesLeft :: Color -> Board -> Bool
anyMovesLeft c b = any (pieceCanMove b) op
                 where op = findAllInBoard (\t -> t /= Nothing && tileColor t == c) b

moveCausesCheck :: Board -> Color -> Coord -> Coord -> Bool
moveCausesCheck b c p1 p2 | not straight && not diagonal = False -- If there isn't a straight line between moved piece and king, can't cause check
                          | kv == mv                     = False -- If piece moved along line between it and king, can't cause check
                          | threatenPiece  == Nothing    = False -- If no piece is behind the moved piece, can't cause check
                          | threatenColor  == c          = False -- Can't be threatened by your own piece
                          | threatenType   == Queen      = True  -- Queen can threaten either diagonally or straight
                          | straightThreat               = True  -- Threat from a rook
                          | diagonalThreat               = True  -- Threat from a bishop
                          | otherwise                    = False -- Not threatening piece (Knight, pawn etc.)
                          where kp             = findKing b c
                                Vector dkx dky = p1 - kp
                                straight       = dkx == 0 || dky == 0
                                diagonal       = abs dkx == abs dky
                                kv             = Vector (signum dkx) (signum dky)
                                mv             = Vector (signum dpx) (signum dpy)
                                Vector dpx dpy = p2 - p1
                                threatenPiece  = emptyRaycastPiece b kv (p1 + kv)
                                threatenType   = tileType  threatenPiece
                                threatenColor  = tileColor threatenPiece
                                straightThreat = straight && threatenType == Rook
                                diagonalThreat = diagonal && threatenType == Bishop
