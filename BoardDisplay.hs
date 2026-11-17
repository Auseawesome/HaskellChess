module BoardDisplay() where
import ChessUtils
import Piece
import PieceDisplay
import Board

-- The padding on either side of a piece on the board
piecePadding :: Int
piecePadding = 2

getSpacePadding :: String
getSpacePadding = replicate piecePadding ' '

bufferLines :: Int
bufferLines = 1

intersperse :: a -> [a] -> [a]
intersperse j (x:xs) = foldl (\a e -> a ++ [j, e]) [x] xs

interMultiple :: Int -> a -> [a] -> [a]
interMultiple i j (x:xs) = foldl (\a e -> a ++ (iterate (j :) [e] !! i)) [x] xs

interBuffer :: a -> [a] -> [a]
interBuffer = interMultiple bufferLines

intercalate :: [a] -> [[a]] -> [a]
intercalate j xs = concat $ intersperse j xs

surround :: [a] -> [a] -> [a]
surround o i = o++i++o

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = s : chunk n e
             where (s,e) = splitAt n xs

dividingLine :: String
dividingLine = replicate (8*(piecePadding*2+3)+2) '-'

bufferLine :: String
bufferLine = "  |" ++ (intercalate "|" $ replicate 8 $ replicate (2+piecePadding*2) ' ')

displayLine :: [Tile] -> String
displayLine = intercalate "|" . map ((surround getSpacePadding) . tileCode)

numberRows :: Int -> [String] -> [String]
numberRows _ []     = []
numberRows i (x:xs) = ((show i)++" |"++x):(numberRows (i+1) xs)

-- Generates the file headers for the board
headerLine :: String
headerLine = h5
           where h1 = ['a'..'h']                       -- Generates header letters for board, ranging from a to h
                 h2 = map (: ' ' : getSpacePadding) h1 -- Pads the letters with three spaces afterwards
                 h3 = map (getSpacePadding ++)      h2 -- Adds two extra spaces to the start to fit even width
                 h4 = intercalate "|"               h3 -- Inserts dividing lines between the letters
                 h5 = "  |" ++                      h4 -- Prepends a column for the row numbers

-- Displays the board as a string
displayBoard :: Board -> String
displayBoard b = concat d8
               where bl = replicate bufferLines bufferLine
                     c  = turn b
                     ch = chunk 8 $ grid b                 -- Chunks the board into a list of rows
                     d1 = map displayLine               ch -- Applies displayLine to each row to add padding and dividing vertical lines
                     d2 = numberRows 1                  d1 -- Adds the row numbers
                     d3 = if' (c == White) (reverse d2) d2 -- Flip board so white is at bottom on white's turn
                     d4 = intersperse dividingLine      d3 -- Adds dividing horizontal lines to each row
                     d5 = interBuffer bufferLine        d4 -- Adds buffer lines between each row and divider
                     d6 = surround bl                   d5 -- Adds extra buffer lines to the outsides
                     d7 = d6 ++ [dividingLine, headerLine] -- Adds a dividing line and file header at the bottom
                     d8 = map (++"\n")                  d7 -- Adds new lines to the end of every line

instance Show Board where
      show = displayBoard