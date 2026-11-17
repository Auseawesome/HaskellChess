module DisplayConfig(
    BoardDC(BoardDC, piecePadding, bufferLines),

    defaultDC
) where

data BoardDC = BoardDC {
    piecePadding :: Int,
    bufferLines :: Int
}

defaultDC :: BoardDC
defaultDC = BoardDC 2 1