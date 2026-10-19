module Board(
  Board(grid),
  Coord,
  hasKingMoved, hasRookMoved, hasPawnDoubled,
  flagKingMoved, flagRookMoved,
  onBoard,
  getPiece, setPiece, movePiece,
  isEmpty, isAlly, isOpposing,
  makeBoard
) where
import Piece

data Board = Board {
  grid        :: [Tile],
  kingMoved   :: (Bool, Bool),
  rookMoved   :: ((Bool, Bool), (Bool, Bool)),
  pawnDoubled :: (Maybe Int, Maybe Int)
}
type Coord = (Int, Int)

boardColorData :: (a, a) -> Color -> a
boardColorData (d, _) White = d
boardColorData (_, d) Black = d

hasKingMoved :: Board -> Color -> Bool
hasKingMoved b c = boardColorData (kingMoved b) c

hasRookMoved :: Board -> Color -> Coord -> Bool
hasRookMoved b c (x, _) | x == 0    = d1
                        | x == 8    = d2
                        | otherwise = True
                        where (d1, d2) = boardColorData (rookMoved b) c

hasPawnDoubled :: Board -> Color -> Coord -> Bool
hasPawnDoubled b c (x, _) = boardColorData (pawnDoubled b) c == Just x

flagKingMoved :: Board -> Color -> Board
flagKingMoved (Board g (_, bm) r p) White = Board g (True, bm) r p
flagKingMoved (Board g (wm, _) r p) Black = Board g (wm, True) r p

getNewRookFlag :: (Bool, Bool) -> Int -> (Bool, Bool)
getNewRookFlag (f1, f2) x | x == 0    = (True, f2)
                          | x == 8    = (f1, True)
                          | otherwise = (f1, f2)

flagRookMoved :: Board -> Color -> Coord -> Board
flagRookMoved b c (x, _) | c == White = Board g k (wf', bf) p
                         | c == Black = Board g k (bf, wf') p
                         | otherwise  = b
                         where (Board g k (wf, bf) p) = b
                               wf'                    = getNewRookFlag wf x
                               bf'                    = getNewRookFlag bf x

onBoard :: Coord -> Bool
onBoard (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

index :: Coord -> Int
index (x, y) = x + y * 8

validIndex :: Coord -> Maybe Int
validIndex p | not $ onBoard p = Nothing
             | otherwise = Just $ index p

getPiece :: Board -> Coord -> Tile
getPiece b p = do p' <- validIndex p
                  (grid) b !! p'

setPiece :: Board -> Coord -> Tile -> Board
setPiece b p t | not $ onBoard p = b
               | otherwise = Board (x ++ (t : ys)) (kingMoved b) (rookMoved b) (pawnDoubled b)
               where (x,_:ys) = splitAt (index p) (grid b)

movePiece :: Board -> Coord -> Coord -> Board
movePiece b p1 p2 = setPiece (setPiece b p2 $ getPiece b p1) p1 Nothing

isEmpty :: Board -> Coord -> Bool
isEmpty b p = getPiece b p == Nothing

isAlly :: Board -> Color -> Coord -> Bool
isAlly b c1 p | isEmpty b p = False
              | otherwise = c1 == c2
              where (Just (Piece c2 _)) = getPiece b p

isOpposing :: Board -> Color -> Coord -> Bool
isOpposing b c1 p | isEmpty b p = False
                  | otherwise = c1 /= c2
                  where (Just (Piece c2 _)) = getPiece b p

backRankPiece :: Color -> Int -> Tile
backRankPiece c 0 = Just $ Piece c Rook
backRankPiece c 1 = Just $ Piece c Knight
backRankPiece c 2 = Just $ Piece c Bishop
backRankPiece c 3 = Just $ Piece c Queen
backRankPiece c 4 = Just $ Piece c King
backRankPiece c 5 = Just $ Piece c Bishop
backRankPiece c 6 = Just $ Piece c Knight
backRankPiece c 7 = Just $ Piece c Rook
backRankPiece _ _ = Nothing

defaultPiece :: Coord -> Tile
defaultPiece (x, 0) = backRankPiece White x
defaultPiece (_, 1) = Just $ Piece White Pawn
defaultPiece (x, 7) = backRankPiece Black x
defaultPiece (_, 6) = Just $ Piece Black Pawn
defaultPiece (_, _) = Nothing

makeBoard :: Board
makeBoard = Board grid (False, False) ((False, False), (False, False)) (Nothing, Nothing)
            where grid     = map defaultPiece [(x,y) |  y <- [0..7], x <- [0..7]]