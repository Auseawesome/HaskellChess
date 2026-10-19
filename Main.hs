module Main(main) where
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move

moveLoop :: Board -> IO()
moveLoop b = do
             putStrLn $ displayBoard b

             x1 <- readLn
             y1 <- readLn

             x2 <- readLn
             y2 <- readLn

             moveLoop $ makeMove b White (x1, y1) (x2, y2)


main :: IO()
main = do
    let board = makeBoard
    moveLoop board

