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
chessToCoord (x:y:"") = do x' <- charToFile x
                           y' <- charToRank y

                           let p = (x', y')

                           if' (onBoard p)
                               (return p)
                               (Left "That position is not on the board!")
chessToCoord s        = Left  $ (shows s) " is not a valid coordinate."

-- Converts a character to a file number, returning a string error if it isn't valid
charToFile :: Char -> OrError Int
charToFile 'a' = Right 0
charToFile 'b' = Right 1
charToFile 'c' = Right 2
charToFile 'd' = Right 3
charToFile 'e' = Right 4
charToFile 'f' = Right 5
charToFile 'g' = Right 6
charToFile 'h' = Right 7
charToFile  c  = Left (c:" is not a valid file.")

-- Converts a character to a rank number, returning a string error if it isn't valid
charToRank :: Char -> OrError Int
charToRank '0' = Right 0
charToRank '1' = Right 1
charToRank '2' = Right 2
charToRank '3' = Right 3
charToRank '4' = Right 4
charToRank '5' = Right 5
charToRank '6' = Right 6
charToRank '7' = Right 7
charToRank  c  = Left (c:" is not a valid rank.")

-- On error, takes action to retry and retries after printing the error
returnOrRetry :: IO a -> OrError a -> IO a
returnOrRetry _ (Right x) = return x
returnOrRetry m (Left  e) = putStrLn e >> m

inputCoord :: String -> IO Coord
inputCoord ord = do
              putStrLn ("Input the "++ord++" x-coordinate (eg. a1): ")
              p <- getLine
              let p' = chessToCoord p

              returnOrRetry (inputCoord ord) p'

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

