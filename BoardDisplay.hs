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

displayBoard :: Board -> String
displayBoard b = concat $
                 (:) ("  |" ++ (intercalate "|" [' ':(show x)++"  " | x <- [0..7]]) ++ "\n") $
                 (:) (dividingLine ++ "\n") $
                 map (++"\n") $
                 surround [bufferLine] $
                 intersperse bufferLine $
                 intersperse dividingLine $
                 numberRows 0 $
                 map displayLine (chunk 8 (grid b))