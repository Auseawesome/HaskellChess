module PieceDisplay(tileName) where
import Piece

tileName :: Tile -> String
tileName Nothing            = "Empty"
tileName (Just (Piece c t)) = show c ++ ' ' : show t