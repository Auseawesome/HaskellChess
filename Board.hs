module Board(Board, onBoard, getPiece, makeBoard) where
import Piece

type Board = [Tile]

onBoard :: (Int, Int) -> Bool
onBoard (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

index :: (Int, Int) -> Int
index (x, y) = x + y * 8

getPiece :: Board -> (Int, Int) -> Tile
getPiece b p | not $ onBoard p = Nothing
             | otherwise = b !! index p

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