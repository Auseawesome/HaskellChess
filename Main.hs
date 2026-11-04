module Main(main) where
import ChessUtils
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move
import Error

-- Converts a chess-tile string to a coordinate, returning a string error if it isn't valid
chessToCoord :: String -> OrError Coord
chessToCoord ""       = errLn "An empty string is not a valid coordinate."
chessToCoord (x:y:"") = do x' <- charToFile x
                           y' <- charToRank y

                           let p = (x', y')

                           if' (onBoard p)
                               (noErr p)
                               (errLn "That position is not on the board!")
chessToCoord s        = errLn $ shows s " is not a valid coordinate."

-- Converts a character to a file number, returning a string error if it isn't valid
charToFile :: Char -> OrError Int
charToFile 'a' = noErr 0
charToFile 'b' = noErr 1
charToFile 'c' = noErr 2
charToFile 'd' = noErr 3
charToFile 'e' = noErr 4
charToFile 'f' = noErr 5
charToFile 'g' = noErr 6
charToFile 'h' = noErr 7
charToFile  c  = errLn (c:" is not a valid file.")

-- Converts a character to a rank number, returning a string error if it isn't valid
charToRank :: Char -> OrError Int
charToRank '1' = noErr 0
charToRank '2' = noErr 1
charToRank '3' = noErr 2
charToRank '4' = noErr 3
charToRank '5' = noErr 4
charToRank '6' = noErr 5
charToRank '7' = noErr 6
charToRank '8' = noErr 7
charToRank  c  = errLn (c:" is not a valid rank.")

inputCoord :: String -> IO Coord
inputCoord msg = do
              putStrLn msg
              p <- getLine
              let p' = chessToCoord p

              returnOrRetry (inputCoord msg) p'

inputMove :: Color -> Board -> IO (Coord, Coord)
inputMove c b = do
             p1 <- inputCoord "Input the position of the piece you want to move (eg. d2): "

             p2 <- inputCoord "Input the space to move it to (eg. d4): "

             if' (isValidMove b c p1 p2)
                 (return (p1, p2))
                 (putStrLn "That move is invalid, please try another one!" >> inputMove c b)

moveNoCheck :: Board -> IO Board
moveNoCheck b = do let c  = turn b
                   (p1, p2) <- inputMove c b
                   let b' = makeMove c p1 p2 b

                   if' (currentlyInCheck b')
                       (putStrLn "That move would leave you in check!" >> moveNoCheck b)
                       (return b')

moveLoop :: Board -> IO()
moveLoop b = do let c = turn b
                putStrLn ("It is "++(show c)++"'s Turn\n")
                if' (currentlyInCheck b) (putStrLn "You are in check!") (return ())
                putStrLn $ displayBoard b

                b' <- moveNoCheck b

                moveLoop $ nextTurn b'

main :: IO()
main = do
    let board = makeBoard
    moveLoop board

