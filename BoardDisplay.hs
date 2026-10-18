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
dividingLine = replicate (8*4 + 7) '-'

bufferLine :: String
bufferLine = intercalate "|" $ replicate 8 "    "

displayLine :: [Tile] -> String
displayLine ts = intercalate "|" $ map ((surround " ").tileCode) ts

displayBoard :: Board -> String
displayBoard b = concat $
                 map (++"\n") $
                 surround [bufferLine] $
                 intersperse bufferLine $
                 intersperse dividingLine $
                 map displayLine (chunk 8 (grid b))