module PieceDisplay(tileName) where
import Piece

pieceName :: Piece -> String
pieceName (Piece c t) = show c ++ ' ' : show t

tileName :: Tile -> String
tileName Nothing  = "Empty"
tileName (Just p) = pieceName p

colorCode :: Color -> Char
colorCode White = 'w'
colorCode Black = 'b'

pieceCode :: PieceType -> Char
pieceCode Pawn   = 'p'
pieceCode Bishop = 'b'
pieceCode Knight = 'k'
pieceCode Rook   = 'r'
pieceCode Queen  = 'Q'
pieceCode King   = 'K'