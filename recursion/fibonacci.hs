{-
  (C) David Lettier 2016
  http://www.lettier.com/

  Write the Fibonacci function.
-}

import System.Environment (getArgs)

fib :: Int -> Int
fib x | x <= 0 = 0
fib 1 = 1
fib x = (+) (fib y) (fib z)
  where y = x - 1
        z = x - 2

main :: IO ()
main = do
  args <- getArgs
  let args' = map (\ x -> read x :: Int) args
  let x = case args' of
        [] -> Nothing
        (x:y) -> Just x
  let result = case x of
        Nothing -> "Error."
        Just x -> show $ fib x
  putStrLn result
  return ()
