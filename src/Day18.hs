{-# LANGUAGE LambdaCase #-}

module Day18 where

import Paths_AOC2021
import Data.Char (digitToInt)
import Paths_AOC2021
import Data.List (foldl', foldl1')
import Paths_AOC2021
import Debug.Trace (trace)

type Level = [Side]

data Side = L | R deriving (Show, Eq, Ord)

type SNNum = (Level, Int)

instance Enum Side where
  fromEnum L = 0
  fromEnum R = 1
  toEnum n = case n `mod` 2 of
    0 -> L
    1 -> R

parseSNNum :: Level -> String -> [SNNum]
parseSNNum _ [] = []
parseSNNum n ('[' : xs) = parseSNNum (L : n) xs
parseSNNum n (',' : xs) = parseSNNum (R : n) xs
parseSNNum n (']' : xs) = parseSNNum (tail n) xs
parseSNNum n (x : xs) = (n, digitToInt x) : parseSNNum (tail n) xs

addSNNum :: [SNNum] -> [SNNum] -> [SNNum]
addSNNum xs ys = map (f L) xs ++ map (f R) ys
  where
    f s (l, i) = (l ++ [s], i)

explode :: [SNNum] -> [SNNum] -> [SNNum]
-- explode xs ys | trace (show (reverse ys ++ xs)) False = undefined
explode ((L : ls, x1) : (R : rs, x2) : xs) ys
  | length ls >= 4 && ls == rs =
      let rhs = case xs of
            [] -> []
            (s3, x3) : r -> (s3, x3 + x2) : r
          lhs = case ys of
            [] -> []
            (s0, x0) : l -> (s0, x0 + x1) : l
       in explode (reverse lhs ++ (head ls : tail ls, 0) : rhs) []
explode (x : xs) ys = explode xs (x : ys)
explode [] ys = split (reverse ys) []

split :: [SNNum] -> [SNNum] -> [SNNum]
split ((s, x) : xs) ys | x > 9 = explode (reverse ys ++ (L : s, x `div` 2) : (R : s, (x `div` 2) + (x `mod` 2)) : xs) []
split (x : xs) ys = split xs (x : ys)
split [] ys = reverse ys

magnitude :: SNNum -> Int
magnitude (s, i) = i * product (map (\case L -> 3; R -> 2) s)

day18 :: IO ()
day18 = do
  -- input <- map (parseSNNum []) . lines <$> readFile "input/test18.txt"
  input <- map (parseSNNum []) . lines <$> readFile "input/input18.txt"
  putStrLn $ ("day18a: " ++) $ show $ sum $ map magnitude $ foldl1' (\acc -> (`explode` []) . addSNNum acc) input
  putStrLn $ ("day18b: " ++) $ show $
    maximum $
      [ (sum . map magnitude) (explode (addSNNum x y) [])
        | x <- input,
          y <- input,
          x /= y
      ]
