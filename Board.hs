module Board(Board, onBoard, getPiece, setPiece, movePiece, isEmpty, isOpposing, makeBoard) where
import Piece

type Board = [Tile]
type Coord = (Int, Int)

onBoard :: Coord -> Bool
onBoard (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

index :: Coord -> Int
index (x, y) = x + y * 8

validIndex :: Coord -> Maybe Int
validIndex p | not $ onBoard p = Nothing
             | otherwise = Just $ index p

getPiece :: Board -> Coord -> Tile
getPiece b p = do p' <- validIndex p
                  b !! p'

setPiece :: Board -> Coord -> Tile -> Board
setPiece b p t | not $ onBoard p = b
               | otherwise = x ++ (t : ys)
                 where (x,_:ys) = splitAt (index p) b

movePiece :: Board -> Coord -> Coord -> Board
movePiece b p1 p2 = setPiece (setPiece b p2 $ getPiece b p1) p1 Nothing

isEmpty :: Board -> Coord -> Bool
isEmpty b p = getPiece b p == Nothing

isOpposing :: Board -> Color -> Coord -> Bool
isOpposing b c1 p | isEmpty b p = False
                  | c2 == c1    = False
                  | otherwise   = True
                  where (Just (Piece c2 _)) = getPiece b p

backRankPiece :: Color -> Int -> Tile
backRankPiece c 0 = Just $ Piece c Rook
backRankPiece c 1 = Just $ Piece c Knight
backRankPiece c 2 = Just $ Piece c Bishop
backRankPiece c 3 = Just $ Piece c Queen
backRankPiece c 4 = Just $ Piece c King
backRankPiece c 5 = Just $ Piece c Bishop
backRankPiece c 6 = Just $ Piece c Knight
backRankPiece c 7 = Just $ Piece c Rook
backRankPiece _ _ = Nothing

defaultPiece :: Coord -> Tile
defaultPiece (x, 0) = backRankPiece White x
defaultPiece (_, 1) = Just $ Piece White Pawn
defaultPiece (x, 7) = backRankPiece Black x
defaultPiece (_, 6) = Just $ Piece Black Pawn
defaultPiece (_, _) = Nothing

makeBoard :: Board
makeBoard = map defaultPiece [(x,y) |  y <- [0..7], x <- [0..7]]