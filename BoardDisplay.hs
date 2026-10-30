module BoardDisplay(displayBoard) where
import Piece
import PieceDisplay
import Board

intersperse :: a -> [a] -> [a]
intersperse j (x:xs) = foldl (\a e -> a ++ [j, e]) [x] xs

intercalate :: [a] -> [[a]] -> [a]
intercalate j xs = concat $ intersperse j xs

surround :: [a] -> [a] -> [a]
surround o i = o++i++o

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = s : chunk n e
             where (s,e) = splitAt n xs

dividingLine :: String
dividingLine = replicate (8*4 + 10) '-'

bufferLine :: String
bufferLine = "  |" ++ (intercalate "|" $ replicate 8 "    ")

displayLine :: [Tile] -> String
displayLine ts = intercalate "|" $ map ((surround " ").tileCode) ts

numberRows :: Int -> [String] -> [String]
numberRows _ []     = []
numberRows i (x:xs) = ((show i)++" |"++x):(numberRows (i+1) xs)

headerLine :: String
headerLine = "  |" ++ h5 ++ "\n"
           where h1 = [0..7]
                 h2 = map show           h1
                 h3 = map (surround " ") h2
                 h4 = map (++ " ")       h3
                 h5 = intercalate "|"    h4

displayBoard :: Board -> String
displayBoard b = concat d8
               where d1 = map displayLine (chunk 8 (grid b))
                     d2 = numberRows 0             d1
                     d3 = intersperse dividingLine d2
                     d4 = intersperse bufferLine   d3
                     d5 = surround [bufferLine]    d4
                     d6 = map (++"\n")             d5
                     d7 = (dividingLine ++ "\n") : d6
                     d8 = headerLine :             d7