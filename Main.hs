module Main(main) where
import Piece
import PieceDisplay
import Board
import BoardDisplay

main = do
    let board = makeBoard
    putStrLn $ displayBoard board
    
    let newBoard = movePiece board (0, 0) (2, 3)
    putStrLn $ displayBoard newBoard

    print $ isOpposing newBoard White (2, 3)

