{-
  (C) David Lettier 2016
  http://www.lettier.com/

  Write the least common multiple function.
-}

import System.Environment (getArgs)

lcm' :: Int -> Int -> Maybe Int
lcm' 0 0 = Just 0
lcm' 0 _ = Nothing
lcm' _ 0 = Nothing
lcm' x y = Just $ head $ filter (\ z -> (mod z b) == 0) multiples_a
  where multiples_a = multiples a
        a = abs x
        b = abs y

multiples :: Int -> [Int]
multiples x = map (* x) [1..]

main :: IO ()
main = do
  args <- getArgs
  let args' = map (\ x -> read x :: Int) args
  let [x, y] = case args' of
        []      -> [0, 1]
        [x]     -> [0, x]
        (x:y:z) -> [x, y]
  let result = case lcm' x y of
        Nothing -> "Error."
        Just x -> show x
  putStrLn result
  return ()
