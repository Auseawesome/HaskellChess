module Main(main) where
import ChessUtils
import Board
import DisplayConfig
import BoardDisplay -- BoardDisplay contains Show instance for Board
import Move
import Error

inputCoord :: String -> IO Coord
inputCoord msg = do
              putStrLn msg
              p <- getLine
              let p' = chessToCoord p

              retryOrReturn (inputCoord msg) p'

printInputError :: Bool -> Bool -> IO ()
printInputError pe vc = do let nm = not pe && vc -- It has errored, if not either empty error or wrong color, must be no moves

                           if' nm
                               (putStrLn "There area no possible moves with that piece, please try another one!")
                               $ if' pe
                                     (putStrLn "You cannot move a non-existent piece!")
                                     (putStrLn "You cannot move your opponent's piece!")


inputFirstCoord :: Board -> IO (Coord, [Coord])
inputFirstCoord b = do p <- inputCoord "Input the position of the piece you want to move (eg. d2): "
                       
                       let pe = getTile b p == Nothing
                       let vc = colorAt b p == turn b
                       let posMoves   = validMoves b p

                       if' (posMoves /= [] && not pe && vc)
                           (return (p, posMoves))
                           (printInputError pe vc >> inputFirstCoord b)

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

yesNoInput :: IO Bool
yesNoInput = do str <- getLine
                let maybeOut = r str
                let Just out = maybeOut
                if' (maybeOut == Nothing)
                    (putStrLn "Please input either y or n" >> yesNoInput)
                    (return out)
             where r :: String -> Maybe Bool
                   r s = case s of
                              ('y':_)   -> Just True
                              ('Y':_)   -> Just True
                              ('n':_)   -> Just False
                              ('N':_)   -> Just False
                              otherwise -> Nothing                  

makeConfBoard :: IO Board
makeConfBoard = do putStrLn "Do you want to use the default board display? (y/n)"
                   useDefault <- yesNoInput
                   
                   return $ makeBoard defaultDC

main :: IO()
main = do
    board <- makeConfBoard
    moveLoop board

