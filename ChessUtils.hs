module ChessUtils(if', chessToCoord) where
import Board
import Error

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ x = x

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