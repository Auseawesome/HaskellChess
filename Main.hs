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
chessToCoord ""       = Left  "An empty string is not a valid coordinate."
chessToCoord (c:"")   = Left  (c:" is not a valid coordinate.")
chessToCoord (x:y:"") = do x' <- charToFile x
                           y' <- charToRank y

                           let p = (x', y')

                           if' (onBoard p)
                               (return p)
                               (Left "That position is not on the board!")
chessToCoord s        = Left  $ (shows s) " is not a valid coordinate."

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
charToRank '0' = noErr 0
charToRank '1' = noErr 1
charToRank '2' = noErr 2
charToRank '3' = noErr 3
charToRank '4' = noErr 4
charToRank '5' = noErr 5
charToRank '6' = noErr 6
charToRank '7' = noErr 7
charToRank  c  = errLn (c:" is not a valid rank.")

inputCoord :: String -> IO Coord
inputCoord msg = do
              putStrLn ("Input the "++msg++" (eg. a1): ")
              p <- getLine
              let p' = chessToCoord p

              returnOrRetry (inputCoord msg) p'

inputMove :: Color -> Board -> IO (Coord, Coord)
inputMove c b = do
             p1 <- inputCoord "position of the piece you want to move"

             p2 <- inputCoord "space to move it to"

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

