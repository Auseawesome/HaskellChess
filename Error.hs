module Error(OrError, returnOrRetry, errLn, noErr) where

type OrError = Either String

-- On error, takes action to retry and retries after printing the error
returnOrRetry :: IO a -> OrError a -> IO a
returnOrRetry _ (Right x) = return x
returnOrRetry m (Left  e) = putStrLn e >> m

-- Synonym of 'Left' for errors, helps readability
errLn :: String -> OrError a
errLn = Left

-- Synonym of 'Right' for errors, helps readability
noErr :: a -> OrError a
noErr = Right