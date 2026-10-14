data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Show, Eq)

data Player = White | Black
    deriving (Show, Eq)

data Tile = Empty | Piece (PieceType) (Player)
    deriving (Show, Eq)

main = do
    let test = Piece Pawn White
    print test
