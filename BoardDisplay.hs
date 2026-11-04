module BoardDisplay(displayBoard) where
import ChessUtils
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
displayLine = intercalate "|" . map ((surround " ") . tileCode)

numberRows :: Int -> [String] -> [String]
numberRows _ []     = []
numberRows i (x:xs) = ((show i)++" |"++x):(numberRows (i+1) xs)

-- Generates the file headers for the board
headerLine :: String
headerLine = h5
           where h1 = ['a'..'h']         -- Generates header letters for board, ranging from a to h
                 h2 = map (: "  ")    h1 -- Pads the letters with two spaces afterwards
                 h3 = map (' ' :)     h2 -- Adds an extra space to the start to fit even width
                 h4 = intercalate "|" h3 -- Inserts dividing lines between the letters
                 h5 = "  |" ++        h4 -- Prepends a column for the row numbers

-- Displays the board as a string
displayBoard :: Board -> Color -> String
displayBoard b c = concat d8
                 where ch = chunk 8 $ grid b                 -- Chunks the board into a list of rows
                       d1 = map displayLine          ch      -- Applies displayLine to each row to add padding and dividing vertical lines
                       d2 = numberRows 1             d1      -- Adds the row numbers
                       d3 = if' (c == White) (reverse d2) d2 -- Flip board so white is at bottom on white's turn
                       d4 = intersperse dividingLine d3      -- Adds dividing horizontal lines to each row
                       d5 = intersperse bufferLine   d4      -- Adds buffer lines between each row and divider
                       d6 = surround [bufferLine]    d5      -- Adds extra buffer lines to the outsides
                       d7 = d6 ++ [dividingLine, headerLine] -- Adds a dividing line and file header at the bottom
                       d8 = map (++"\n")             d7      -- Adds new lines to the end of every line