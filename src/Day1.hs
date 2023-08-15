module Day1 where

import Data.List.Split (divvy)

countIncrease :: Ord a => [a] -> Int
countIncrease = go 0
  where
    go n [] = n
    go n [_] = n
    go n (x : y : xs)
      | y > x = go (n + 1) (y : xs)
      | otherwise = go n (y : xs)

day1 :: IO ()
day1 = do
  input <- map (read @Int) . lines <$> readFile "input/input1.txt"
  putStrLn $ ("day1a: " ++) $ show $ countIncrease input
  putStrLn $ ("day1b: " ++) $ show $ countIncrease $ map sum $ divvy 3 1 input
