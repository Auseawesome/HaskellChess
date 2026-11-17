module DisplayConfig(
    BoardDC(BoardDC, piecePadding, bufferLines),

    defaultDC, compactDC, slimDC, wideDC,

    selectDC
) where

data BoardDC = BoardDC {
    piecePadding :: Int,
    bufferLines :: Int
}

defaultDC, compactDC, slimDC, wideDC :: BoardDC
defaultDC = BoardDC 2 1
compactDC = BoardDC 1 0
slimDC    = BoardDC 1 1
wideDC    = BoardDC 3 1

selectDC :: Char -> Maybe BoardDC
selectDC 'd' = Just defaultDC
selectDC 'c' = Just compactDC
selectDC 's' = Just slimDC
selectDC 'w' = Just wideDC
selectDC  _  = Nothing