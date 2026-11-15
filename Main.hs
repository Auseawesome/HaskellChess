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

inputMove :: Board -> IO (Coord, Coord)
inputMove b = do p1 <- inputCoord "Input the position of the piece you want to move (eg. d2): "

                 p2 <- inputCoord "Input the space to move it to (eg. d4): "

                 if' (isValidMove b (turn b) p1 p2)
                     (return (p1, p2))
                     (putStrLn "That move is invalid, please try another one!" >> inputMove b)

moveNoCheck :: Board -> IO Board
moveNoCheck b = do (p1, p2) <- inputMove b
                   if' (moveCausesCheck b (turn b) p1 p2)
                       (putStrLn "That move would leave you in check!" >> moveNoCheck b)
                       (return $ makeMove (turn b) p1 p2 b)

moveLoop :: Board -> IO()
moveLoop b = do let c = turn b
                let inCheck   = currentlyInCheck b
                let movesLeft = anyMovesLeft c   b

                if (not movesLeft)
                then do if' inCheck (putStrLn ("Checkmate, " ++ shows c "Wins!")) (putStrLn "Stalemate, the game ends in a draw!")
                else do putStrLn ("It is " ++ shows c "'s Turn\n")
                        if' inCheck (putStrLn "You are in check!") (return ())
                        print b

                        b' <- moveNoCheck b
                        moveLoop $ nextTurn b'

main :: IO()
main = do
    let board = makeBoard
    moveLoop board

