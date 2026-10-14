data PieceType = Pawn | Bishop | Knight | Rook | Queen | King
    deriving (Show, Eq)

data Player = White | Black
    deriving (Show, Eq)

data Piece = Piece { color :: Player, piece_type  :: PieceType }
    deriving (Show, Eq)

type Tile = Maybe Piece

backRankPiece :: Player -> Int -> Tile
backRankPiece c 0 = Just $ Piece c Rook
backRankPiece c 1 = Just $ Piece c Knight
backRankPiece c 2 = Just $ Piece c Bishop
backRankPiece c 3 = Just $ Piece c Queen
backRankPiece c 4 = Just $ Piece c King
backRankPiece c 5 = Just $ Piece c Bishop
backRankPiece c 6 = Just $ Piece c Knight
backRankPiece c 7 = Just $ Piece c Rook
backRankPiece _ _ = Nothing

defaultPiece :: (Int,Int) -> Tile
defaultPiece (x, 0) = backRankPiece White x
defaultPiece (_, 1) = Just $ Piece White Pawn
defaultPiece (x, 7) = backRankPiece Black x
defaultPiece (_, 6) = Just $ Piece Black Pawn
defaultPiece (_, _) = Nothing

main = do
    let indices = [(x,y) |  y <- [0..8], x <- [0..8]]
    let board = map defaultPiece indices
    print board
