module Board(Board, onBoard, getPiece, setPiece, makeBoard) where
import Piece

type Board = [Tile]

onBoard :: (Int, Int) -> Bool
onBoard (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

index :: (Int, Int) -> Int
index (x, y) = x + y * 8

validIndex :: (Int, Int) -> Maybe Int
validIndex p | not $ onBoard p = Nothing
             | otherwise = Just $ index p

getPiece :: Board -> (Int, Int) -> Tile
getPiece b p = do p' <- validIndex p
                  b !! p'

setPiece :: Board -> (Int, Int) -> Tile -> Board
setPiece b p t | not $ onBoard p = b
               | otherwise = x ++ (t : ys)
                 where (x,_:ys) = splitAt (index p) b

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

defaultPiece :: (Int,Int) -> Tile
defaultPiece (x, 0) = backRankPiece White x
defaultPiece (_, 1) = Just $ Piece White Pawn
defaultPiece (x, 7) = backRankPiece Black x
defaultPiece (_, 6) = Just $ Piece Black Pawn
defaultPiece (_, _) = Nothing

makeBoard :: Board
makeBoard = map defaultPiece [(x,y) |  y <- [0..8], x <- [0..8]]