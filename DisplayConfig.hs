module DisplayConfig(
    BoardDC(BoardDC, piecePadding, bufferLines),

    defaultDC, compactDC, slimDC, wideDC
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