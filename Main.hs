module Main(main) where
import ChessUtils
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move

type OrError = Either String

-- Converts a chess-tile string to a coordinate, returning a string error if it isn't valid
chessToCoord :: String -> OrError Coord
chessToCoord ""       = Left  "An empty string is not a valid coordinate."
chessToCoord (c:"")   = Left  (c:" is not a valid coordinate.")
chessToCoord (x:y:"") = do x' <- charToRow x
                           y' <- charToRow y

                           let p = (x', y')

                           if' (onBoard p)
                               (return p)
                               (Left "That position is not on the board!")
chessToCoord s        = Left  $ (shows s) " is not a valid coordinate."

-- Converts a character to a row or column number, returning a string error if it isn't valid
charToRow :: Char -> OrError Int
charToRow 'a' = Right 0
charToRow 'b' = Right 1
charToRow 'c' = Right 2
charToRow 'd' = Right 3
charToRow 'e' = Right 4
charToRow 'f' = Right 5
charToRow 'g' = Right 6
charToRow 'h' = Right 7
charToRow '0' = Right 0
charToRow '1' = Right 1
charToRow '2' = Right 2
charToRow '3' = Right 3
charToRow '4' = Right 4
charToRow '5' = Right 5
charToRow '6' = Right 6
charToRow '7' = Right 7
charToRow  c  = Left (c:" is not a valid row/column.")

inputCoord :: String -> IO Coord
inputCoord ord = do
              putStrLn ("Input the "++ord++" x-coordinate (eg. a1): ")
              p <- getLine
              let p' = chessToCoord p

              case p' of
                   Left  e -> putStrLn e >> inputCoord ord 
                   Right c -> return c

inputMove :: Color -> Board -> IO (Coord, Coord)
inputMove c b = do
             p1 <- inputCoord "1st"

             p2 <- inputCoord "2nd"

             if' (isValidMove b c p1 p2)
                 (return (p1, p2))
                 (putStrLn "That move is invalid, please try another one!" >> inputMove c b)

-- Finds the king of a given colour player within a board
findKing :: Board -> Color -> Coord
findKing b c = p
             where Just p = findInBoard (== Just (Piece c King)) b -- Assumes king must be present, should always be safe

isPlayerInCheck :: Color -> Board -> Bool
isPlayerInCheck c b = isInCheck b c $ findKing b c

moveNoCheck :: Color -> Board -> IO Board
moveNoCheck c b = do
               (p1, p2) <- inputMove c b

               let b' = makeMove c p1 p2 b

               if' (isPlayerInCheck c b')
                   (putStrLn "That move would leave you in check!" >> moveNoCheck c b)
                   (return b')

moveLoop :: Color -> Board -> IO()
moveLoop c b = do
            putStrLn ("It is "++(show c)++"'s Turn\n")
            if' (isPlayerInCheck c b) (putStrLn "You are in check!") (return ())
            putStrLn $ displayBoard b

            b' <- moveNoCheck c b

            moveLoop (switchColor c) $ b'

main :: IO()
main = do
    let board = makeBoard
    moveLoop White board

