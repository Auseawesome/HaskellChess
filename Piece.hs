module Piece(PieceType(Pawn, Bishop, Knight, Rook, Queen, King), Color(White, Black), Piece(Piece), Tile) where

data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Show, Eq)

data Color = White | Black
    deriving (Show, Eq)

data Piece = Piece { color :: Color, piece_type  :: PieceType }
    deriving (Show, Eq)

type Tile = Maybe Piece