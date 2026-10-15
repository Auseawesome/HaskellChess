module Main(main) where
import Piece
import Board

main = do
    let board = makeBoard
    print $ getPiece board (0, 0)
    print $ getPiece board (2, 1)
    print $ getPiece board (5, 7)
    print $ getPiece board (3, 2)
