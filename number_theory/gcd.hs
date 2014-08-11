{-
  (C) David Lettier 2016
  http://www.lettier.com/

  Write the greatest common divisor function.
-}

import System.Environment (getArgs)

gcd' :: Int -> Int -> Int
gcd' 0 y = abs y
gcd' x 0 = abs x
gcd' x y = case divisors a b of
      [] -> -1
      (x:y) -> x
  where a = abs x
        b = abs y

divisors :: Int -> Int -> [Int]
divisors x y = filter (\ z -> ((mod x z) + (mod y z)) == 0)  [a, b..1]
  where a = min x y
        b = if (a - 1) < 0 then 0 else (a - 1)

-- Euclid's GCD Algorithm

gcd'' :: Int -> Int -> Int
gcd'' x 0 = abs x
gcd'' x y = gcd'' b (mod a b)
  where a = abs x
        b = abs y

main :: IO ()
main = do
  args <- getArgs
  let args' = map (\ x -> read x :: Int) args
  let [x, y] = case args' of
        [] -> [0, 0]
        [x] -> [0, x]
        (x:y:z) -> [x, y]
  let result = assemble_output (gcd' x y) (gcd'' x y)
  putStrLn result
  return ()

assemble_output :: Int -> Int -> String
assemble_output x y = (show x) ++ " " ++ (show y)
