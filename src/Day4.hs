module Day4 where

import Data.Foldable (find)
import Data.List (transpose)
import Data.List.Split
import Data.Maybe (fromMaybe, isNothing)
import Paths_AOC2021

type Board = [[Maybe Int]]

isBingo :: Board -> Bool
isBingo b = any (all isNothing) b || any (all isNothing) b'
  where
    b' = transpose b

firstBingo :: [Board] -> [Int] -> Maybe Int
firstBingo _ [] = Nothing
firstBingo b (x : xs) = case find isBingo b' of
  Nothing -> firstBingo b' xs
  Just bingo -> Just $ x * sum (map (sum . map (fromMaybe 0)) bingo)
  where
    b' = map (map (map (\n -> if Just x == n then Nothing else n))) b

lastBingo :: [Board] -> [Int] -> Maybe Int
lastBingo [] _ = Nothing
lastBingo _ [] = Nothing
lastBingo b (n : ns)
  | length b == 1 = firstBingo b (n : ns)
  | otherwise = lastBingo (filter (not . isBingo) b') ns
  where
    b' = map (map (map (\x -> if Just n == x then Nothing else x))) b

day4 :: IO ()
day4 = do
  input <- splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input4.txt"))
  let numbers = map (read @Int) . splitOn "," $ head input
      boards = map (map (map (Just . read @Int) . words) . lines) $ tail input
  putStrLn $ ("day4a: " ++) $ show $ firstBingo boards numbers
  putStrLn $ ("day4b: " ++) $ show $ lastBingo boards numbers
