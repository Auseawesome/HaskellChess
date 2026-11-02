module Error(OrError, returnOrRetry) where

type OrError = Either String

-- On error, takes action to retry and retries after printing the error
returnOrRetry :: IO a -> OrError a -> IO a
returnOrRetry _ (Right x) = return x
returnOrRetry m (Left  e) = putStrLn e >> m