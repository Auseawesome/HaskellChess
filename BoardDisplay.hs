module BoardDisplay() where
import ChessUtils
import Piece
import PieceDisplay
import Board
import DisplayConfig

----------------------------
-- Display Config Helpers --
----------------------------

padForBoard :: Board -> Int
padForBoard b = piecePadding $ disConf b

getSpacePadding :: Board -> String
getSpacePadding b = replicate (padForBoard b) ' '

-------------------------------
-- List Manipulation Helpers --
-------------------------------

intersperse :: a -> [a] -> [a]
intersperse j (x:xs) = foldl (\a e -> a ++ [j, e]) [x] xs

interMultiple :: [a] -> [a] -> [a]
interMultiple j (x:xs) = foldl (\a e -> a ++ j ++ [e]) [x] xs

intercalate :: [a] -> [[a]] -> [a]
intercalate j xs = concat $ intersperse j xs

surround :: [a] -> [a] -> [a]
surround o i = o++i++o

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = s : chunk n e
             where (s,e) = splitAt n xs

------------------------------
-- Board Display Components --
------------------------------

dividingLine :: Board -> String
dividingLine b = replicate (8*((padForBoard b)*2+3)+2) '-'

bufferLine :: Board -> String
bufferLine b = "  |" ++ (intercalate "|" $ replicate 8 $ replicate (2+(padForBoard b)*2) ' ')

displayLine :: Board -> [Tile] -> String
displayLine b = intercalate "|" . map ((surround $ getSpacePadding b) . tileCode)

numberRows :: Int -> [String] -> [String]
numberRows i = zipWith (++) [(show i)++" |" | i <- [1..]]

-- Generates the file headers for the board
headerLine :: Board -> String
headerLine b = h5
             where h1 = ['a'..'h']                         -- Generates header letters for board, ranging from a to h
                   h2 = map (: ' ' : getSpacePadding b) h1 -- Pads the letters with three spaces afterwards
                   h3 = map (getSpacePadding b ++)      h2 -- Adds two extra spaces to the start to fit even width
                   h4 = intercalate "|"                 h3 -- Inserts dividing lines between the letters
                   h5 = "  |" ++                        h4 -- Prepends a column for the row numbers

------------------------------
-- Full Board Display Logic --
------------------------------

-- Displays the board as a string
displayBoard :: Board -> String
displayBoard b = concat d8
               where ri = turn b == White          -- Current turn to use for board orientation
                     rw b = if' ri (reverse b)  b  -- Function to reverse board if it is white's turn
                     bn = bufferLines $ disConf b  -- Number of buffer lines
                     bl = bufferLine            b  -- Buffer line of width according to config
                     bs = replicate bn bl          -- List of buffer lines equal to display config
                     hl = headerLine            b  -- Header line with spacing equal to display config
                     dl = dividingLine          b  -- Dividing line of length required by display config
                     di = displayLine           b  -- Display a line with correct spacing
                     ch = chunk 8 $ grid        b  -- Chunks the board into a list of rows
                     d1 = map di                ch -- Applies displayLine to each row to add padding and dividing vertical lines
                     d2 = numberRows 1          d1 -- Adds the row numbers
                     d3 = rw                    d2 -- Flip board so white is at bottom on white's turn
                     d4 = intersperse   dl      d3 -- Adds dividing horizontal lines to each row
                     d5 = interMultiple bs      d4 -- Adds buffer lines between each row and divider
                     d6 = surround      bs      d5 -- Adds extra buffer lines to the outsides
                     d7 = d6 ++         [dl, hl]   -- Adds a dividing line and file header at the bottom
                     d8 = map (++"\n")          d7 -- Adds new lines to the end of every line

instance Show Board where
      show = displayBoard