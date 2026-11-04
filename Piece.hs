module Piece(
    PieceType(Pawn, Bishop, Knight, Rook, Queen, King),
    Color(White, Black),
    Piece(Piece),
    Tile,

    switchColor
) where

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Show, Eq)

data Color = White | Black
    deriving (Show, Eq)

data Piece = Piece { color :: Color, pieceType :: PieceType }
    deriving (Show, Eq)

type Tile = Maybe Piece

switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White

tileColor :: Tile -> Color
tileColor (Just p) = color p

tileType :: Tile -> PieceType
tileType (Just p) = pieceType p