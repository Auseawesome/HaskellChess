module Main(main) where
import Piece
import Board

main = do
    let board = makeBoard
    print $ getPiece board (0, 0)
    print $ getPiece board (2, 1)
    print $ getPiece board (5, 7)
    print $ getPiece board (3, 2)
    print $ getPiece board (-1, 0)
    let sameBoard = setPiece board (-1, -1) $ Just $ Piece Black Queen
    print $ sameBoard == board
    let newBoard = setPiece board (3, 4) $ Just $ Piece White Pawn
    print $ getPiece sameBoard (3, 4)
    print $ getPiece newBoard (3, 4)
