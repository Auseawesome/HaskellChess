module DisplayConfig(
    BoardDC(BoardDC, piecePadding, bufferLines)
) where

data BoardDC = BoardDC {
    piecePadding :: Int,
    bufferLines :: Int
}