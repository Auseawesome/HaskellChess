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

typeCode :: PieceType -> Char
typeCode Pawn   = 'p'
typeCode Bishop = 'b'
typeCode Knight = 'k'
typeCode Rook   = 'r'
typeCode Queen  = 'Q'
typeCode King   = 'K'

pieceCode :: Piece -> String
pieceCode (Piece c t) = [colorCode c, typeCode t]