module Board(Board, onBoard, getPiece) where
import Piece

type Board = [Tile]

onBoard :: (Int, Int) -> Bool
onBoard (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

getPiece :: Board -> (Int, Int) -> Tile
getPiece b (x, y) | not $ onBoard (x, y) = Nothing
                  | otherwise = b !! (x + y * 8)