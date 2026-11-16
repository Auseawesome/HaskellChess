module Main(main) where
import ChessUtils
import Board
import BoardDisplay -- BoardDisplay contains Show instance for Board
import Move
import Error

inputCoord :: String -> IO Coord
inputCoord msg = do
              putStrLn msg
              p <- getLine
              let p' = chessToCoord p

              retryOrReturn (inputCoord msg) p'

inputFirstCoord :: Board -> IO (Coord, [Coord])
inputFirstCoord b = do p <- inputCoord "Input the position of the piece you want to move (eg. d2): "
                       
                       let pieceValid = getTile b p /= Nothing && colorAt b p == turn b
                       let posMoves   = validMoves b p

                       if' (posMoves /= [] && pieceValid)
                           (return (p, posMoves))
                           (putStrLn "There are no possible moves with that piece, please try another one!" >> inputFirstCoord b)

inputSecondCoord :: [Coord] -> IO Coord
inputSecondCoord cs = do p <- inputCoord "Input the space to move it to (eg. d4): "
                         
                         if' (any (== p) cs)
                             (return p)
                             (putStrLn "That move is invalid, please try another one!" >> inputSecondCoord cs)

moveLoop :: Board -> IO()
moveLoop b = do let c = turn b
                let inCheck   = currentlyInCheck b
                let movesLeft = anyMovesLeft c   b

                if (not movesLeft)
                then do if' inCheck (putStrLn ("Checkmate, " ++ shows c " Wins!")) (putStrLn "Stalemate, the game ends in a draw!")
                else do putStrLn ("It is " ++ shows c "'s Turn\n")
                        if' inCheck (putStrLn "You are in check!") (return ())
                        print b

                        (p1, pm) <- inputFirstCoord  b
                        p2       <- inputSecondCoord pm
                        moveLoop $ nextTurn $ makeMove c p1 p2 b

main :: IO()
main = do
    let board = makeBoard
    moveLoop board

