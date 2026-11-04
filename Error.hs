module Error(OrError, retryOrReturn, isErr, errLn, noErr) where

type OrError = Either String

-- On error, takes action to retry and retries after printing the error
retryOrReturn :: IO a -> OrError a -> IO a
retryOrReturn _ (Right x) = return x
retryOrReturn m (Left  e) = putStrLn e >> m

-- Check if something is an error
isErr :: OrError a -> Bool
isErr (Right _) = False
isErr (Left  _) = True

-- Synonym of 'Left' for errors, helps readability
errLn :: String -> OrError a
errLn = Left

-- Synonym of 'Right' for errors, helps readability
noErr :: a -> OrError a
noErr = Right