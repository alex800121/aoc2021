module Day3 where

import Paths_AOC2021
import Data.Char (digitToInt)
import Paths_AOC2021
import Control.Applicative (liftA2)

filterBitN :: Int -> Ordering -> Int -> [[Int]] -> [Int]
filterBitN n criteria def input
  | len == 1 = head input
  | otherwise = filterBitN (n + 1) criteria def input'
  where
    len' = length $ head input
    len = length input
    ones = sum $ map (!! (n `mod` len')) input
    zeros = len - ones
    filterer = case compare ones zeros of
      EQ -> def
      c -> if c == criteria then 1 else 0
    input' = filter ((== filterer) . (!! (n `mod` len'))) input

day3 :: IO ()
day3 = do
  input <- map (map digitToInt) . lines <$> (getDataDir >>= readFile . (++ "/input/input3.txt"))
  let len = length input
      a = map (fromEnum . (> 500)) $ foldr (zipWith (+)) (repeat 0) input
      b = map (fromEnum . not . toEnum) a
      ans_a = foldl (\acc x -> x + (2 * acc)) 0 a
      ans_b = foldl (\acc x -> x + (2 * acc)) 0 b
      a' = filterBitN 0 GT 1 input
      b' = filterBitN 0 LT 0 input
      ans_a' = foldl (\acc x -> x + (2 * acc)) 0 a'
      ans_b' = foldl (\acc x -> x + (2 * acc)) 0 b'
  putStrLn $ ("day3a: " ++) $ show (ans_a * ans_b)
  putStrLn $ ("day3b: " ++) $ show (ans_a' * ans_b')
