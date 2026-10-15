module Main(main) where
import Piece
import PieceDisplay
import Board

main = do
    let board = makeBoard
    print $ tileName $ getPiece board (0, 0)
    print $ tileName $ getPiece board (2, 1)
    print $ tileName $ getPiece board (5, 7)
    print $ tileName $ getPiece board (3, 2)
    print $ tileName $ getPiece board (-1, 0)
    let sameBoard = setPiece board (-1, -1) $ Just $ Piece Black Queen
    print $ sameBoard == board
    let newBoard = setPiece board (3, 4) $ Just $ Piece White Pawn
    print $ tileName $ getPiece sameBoard (3, 4)
    print $ tileName $ getPiece newBoard (3, 4)
