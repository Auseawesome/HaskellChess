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

-- Generates the number headers for the board
headerLine :: String
headerLine = h6
           where h1 = [0..7]                -- Generates header numbers for board, ranging from 0 to 7
                 h2 = map show           h1 -- Turns header numbers into strings using show
                 h3 = map (surround " ") h2 -- Pads the numbers with a space either side
                 h4 = map (++ " ")       h3 -- Adds an extra space to the end to fit even width
                 h5 = intercalate "|"    h4 -- Inserts dividing lines between the numbers
                 h6 = "  |" ++ h5 ++ "\n"   -- Prepends a column for the row numbers and adds a new line

displayBoard :: Board -> String
displayBoard b = concat d8
               where c  = chunk 8 $ grid b
                     d1 = map displayLine          c
                     d2 = numberRows 0             d1
                     d3 = intersperse dividingLine d2
                     d4 = intersperse bufferLine   d3
                     d5 = surround [bufferLine]    d4
                     d6 = map (++"\n")             d5
                     d7 = (dividingLine ++ "\n") : d6
                     d8 = headerLine :             d7