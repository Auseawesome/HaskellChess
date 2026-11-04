module Main(main) where
import ChessUtils
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move
import Error

inputCoord :: String -> IO Coord
inputCoord msg = do
              putStrLn msg
              p <- getLine
              let p' = chessToCoord p

              returnOrRetry (inputCoord msg) p'

inputMove :: Board -> IO (Coord, Coord)
inputMove b = do p1 <- inputCoord "Input the position of the piece you want to move (eg. d2): "

                 p2 <- inputCoord "Input the space to move it to (eg. d4): "

                 if' (isValidMove b (turn b) p1 p2)
                     (return (p1, p2))
                     (putStrLn "That move is invalid, please try another one!" >> inputMove b)

moveNoCheck :: Board -> IO Board
moveNoCheck b = do (p1, p2) <- inputMove b
                   let b' = makeMove (turn b) p1 p2 b

                   if' (currentlyInCheck b')
                       (putStrLn "That move would leave you in check!" >> moveNoCheck b)
                       (return b')

moveLoop :: Board -> IO()
moveLoop b = do putStrLn ("It is " ++ shows (turn b) "'s Turn\n")
                if' (currentlyInCheck b) (putStrLn "You are in check!") (return ())
                putStrLn $ displayBoard b

                b' <- moveNoCheck b

                moveLoop $ nextTurn b'

main :: IO()
main = do
    let board = makeBoard
    moveLoop board

