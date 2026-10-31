module Main(main) where
import ChessUtils
import Piece
import PieceDisplay
import Board
import BoardDisplay
import Move

inputCoord :: String -> IO Coord
inputCoord ord = do
              putStrLn ("Input the "++ord++" x-coordinate: ")
              x <- readLn

              putStrLn ("Input the "++ord++" y-coordinate: ")
              y <- readLn

              if' (onBoard (x, y))
                  (return (x, y))
                  (putStrLn "That coordinate is invalid, please input another one!" >> inputCoord ord)

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

