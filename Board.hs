module Board(
  Board(grid),
  Coord,

  makeCoords, makeBoard,

  hasKingMoved, hasRookMoved, hasPawnDoubled,

  flagKingMoved, flagRookMoved,
  flagPawnDoubled, clearPawnDoubled,

  onBoard, getPiece,
  isEmpty, isAlly, isOpposing,
  findInBoard,

  setPiece, movePiece
) where
import Piece

data Board = Board {
  grid        :: [Tile],
  kingMoved   :: (Bool, Bool),
  rookMoved   :: ((Bool, Bool), (Bool, Bool)),
  pawnDoubled :: (Maybe Int, Maybe Int)
}
type Coord = (Int, Int)

------------------------
-- Board Construction --
------------------------

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

makeCoords :: [Coord]
makeCoords = [(x,y) |  y <- [0..7], x <- [0..7]]

makeBoard :: Board
makeBoard = Board grid (False, False) ((False, False), (False, False)) (Nothing, Nothing)
            where grid     = map defaultPiece makeCoords

---------------------
-- Private Helpers --
---------------------

-- Creates an index into the board grid from a coordinate pair
index :: Coord -> Int
index (x, y) = x + y * 8

-- Checks whether a coordinate is on the board before generating an index
validIndex :: Coord -> Maybe Int
validIndex p | not $ onBoard p = Nothing
             | otherwise = Just $ index p

-- Board state data is stored in pairs, get the state corresponding to the correct side
boardColorData :: (a, a) -> Color -> a
boardColorData (d, _) White = d
boardColorData (_, d) Black = d

-- Updates the flag for a rook moving based on the column it is moving from
-- Assumes default starting board
getNewRookFlag :: (Bool, Bool) -> Int -> (Bool, Bool)
getNewRookFlag (f1, f2) x | x == 0    = (True, f2)
                          | x == 8    = (f1, True)
                          | otherwise = (f1, f2)

-- Use a tile predicate to find the index of a tile in the board grid
-- See 'findInBoard' for getting a coordinate
findInGrid :: (Tile -> Bool) -> Int -> [Tile] -> Maybe Int
findInGrid _ _ []     = Nothing
findInGrid f a (x:xs) | f x       = Just a
                      | otherwise = findInGrid f (a+1) xs 

-------------------
-- State Getters --
-------------------

hasKingMoved :: Board -> Color -> Bool
hasKingMoved b c = boardColorData (kingMoved b) c

hasRookMoved :: Board -> Color -> Int -> Bool
hasRookMoved b c x | x == 0    = d1
                   | x == 8    = d2
                   | otherwise = True
                   where (d1, d2) = boardColorData (rookMoved b) c

hasPawnDoubled :: Board -> Color -> Coord -> Bool
hasPawnDoubled b c (x, _) = boardColorData (pawnDoubled b) c == Just x

-------------------
-- State Setters --
-------------------

flagKingMoved :: Board -> Color -> Board
flagKingMoved b White = b {kingMoved = (True, hasKingMoved b Black)}
flagKingMoved b Black = b {kingMoved = (hasKingMoved b White, True)}

flagRookMoved :: Board -> Color -> Int -> Board
flagRookMoved b c x | c == White = b {rookMoved = (wf', bf)}
                    | c == Black = b {rookMoved = (bf, wf')}
                    | otherwise  = b
                    where (wf, bf) = rookMoved b
                          wf'      = getNewRookFlag wf x
                          bf'      = getNewRookFlag bf x

flagPawnDoubled :: Board -> Color -> Int -> Board
flagPawnDoubled b White x = b {pawnDoubled = (Just x, Nothing)}
flagPawnDoubled b Black x = b {pawnDoubled = (Nothing, Just x)}

clearPawnDoubled :: Board -> Board
clearPawnDoubled (Board g k r _) =  Board g k r (Nothing, Nothing)

--------------------
-- Piece Checkers --
--------------------

onBoard :: Coord -> Bool
onBoard (x, y) = x >= 0 && y >= 0 && x < 8 && y < 8

getPiece :: Board -> Coord -> Tile
getPiece b p = do p' <- validIndex p
                  (grid) b !! p'

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

findInBoard :: (Tile -> Bool) -> Board -> Maybe Coord
findInBoard f b | i == Nothing = Nothing
                | otherwise    = Just (mod i' 8, div i' 8)
                where i       = findInGrid f 0 $ grid b
                      Just i' = i

-------------------
-- Piece Setters --
-------------------

setPiece :: Board -> Coord -> Tile -> Board
setPiece b p t | not $ onBoard p = b
               | otherwise       = b {grid = (x ++ (t : ys))}
               where (x,_:ys) = splitAt (index p) (grid b)

movePiece :: Board -> Coord -> Coord -> Board
movePiece b p1 p2 = setPiece (setPiece b p2 $ getPiece b p1) p1 Nothing

