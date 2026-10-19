module Main(main) where
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move

switchPlayer :: Color -> Color
switchPlayer White = Black
switchPlayer Black = White

moveLoop :: Color -> Board -> IO()
moveLoop c b  = do
                putStrLn $ displayBoard b

                x1 <- readLn
                y1 <- readLn

                x2 <- readLn
                y2 <- readLn

                moveLoop (switchPlayer c) $ makeMove b c (x1, y1) (x2, y2)


main :: IO()
main = do
    let board = makeBoard
    moveLoop White board

