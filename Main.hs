module Main(main) where
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ x = x

switchPlayer :: Color -> Color
switchPlayer White = Black
switchPlayer Black = White

redoCoord :: String -> IO Coord
redoCoord ord = putStrLn "That coordinate is invalid, please input another one!" >> inputCoord ord

inputCoord :: String -> IO Coord
inputCoord ord = do
              putStrLn ("Input the "++ord++" x-coordinate: ")
              x <- readLn

              putStrLn ("Input the "++ord++" y-coordinate: ")
              y <- readLn

              if' (onBoard (x, y)) (return (x, y)) (redoCoord ord)

inputMove :: Color -> Board -> IO (Coord, Coord)
inputMove c b = do
             p1 <- inputCoord "1st"

             p2 <- inputCoord "2nd"

             if' (isValidMove b c p1 p2)
                 (return (p1, p2))
                 (putStrLn "That move is invalid, please try another one!" >> inputMove c b)

moveLoop :: Color -> Board -> IO()
moveLoop c b = do
            putStrLn $ displayBoard b

            (p1, p2) <- inputMove c b

            moveLoop (switchPlayer c) $ makeMove b c p1 p2


main :: IO()
main = do
    let board = makeBoard
    moveLoop White board

