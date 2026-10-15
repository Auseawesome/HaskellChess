module PieceDisplay(tileName) where
import Piece

tileName :: Tile -> String
tileName Nothing            = "Empty"
tileName (Just (Piece c t)) = show c ++ ' ' : show t

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