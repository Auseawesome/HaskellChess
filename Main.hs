module Main(main) where
import ChessUtils
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move

inputCoord :: String -> IO Coord
inputCoord ord = do
              putStrLn ("Input the "++ord++" x-coordinate: ")
              x <- readLn

              putStrLn ("Input the "++ord++" y-coordinate: ")
              y <- readLn

              if' (onBoard (x, y))
                  (return (x, y))
                  (putStrLn "That coordinate is invalid, please input another one!" >> inputCoord ord)

inputMove :: Color -> Board -> IO (Coord, Coord)
inputMove c b = do
             p1 <- inputCoord "1st"

             p2 <- inputCoord "2nd"

             if' (isValidMove b c p1 p2)
                 (return (p1, p2))
                 (putStrLn "That move is invalid, please try another one!" >> inputMove c b)

moveLoop :: Color -> Board -> IO()
moveLoop c b = do
            putStrLn ("It is "++(show c)++"'s Turn\n")
            putStrLn $ displayBoard b

            (p1, p2) <- inputMove c b

            moveLoop (switchColor c) $ makeMove b c p1 p2


main :: IO()
main = do
    let board = makeBoard
    moveLoop White board

