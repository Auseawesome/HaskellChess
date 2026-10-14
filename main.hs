data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Show, Eq)

data Player = White | Black
    deriving (Show, Eq)

data Piece = Piece { color :: Player, piece_type  :: PieceType }
    deriving (Show, Eq)

type Tile = Maybe Piece

main = do
    let test = Piece White Pawn
    print test
