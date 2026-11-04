module Piece(
    PieceType(Pawn, Bishop, Knight, Rook, Queen, King),
    Color(White, Black),
    Piece(Piece),
    Tile,

    switchColor,

    tileColor, tileColor',
    tileType, tileType'
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

tileColor' :: Tile -> Maybe Color
tileColor' t = do p <- t
                  return $ color p

tileType :: Tile -> PieceType
tileType (Just p) = pieceType p

tileType' :: Tile -> Maybe PieceType
tileType' t = do p <- t
                 return $ pieceType p