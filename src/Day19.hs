module Day19 where

import Control.Applicative (Alternative (empty))
import Data.List (intersect, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Char (isDigit)
import Control.Monad (guard)

type Index = (Int, Int, Int)

rotate :: Index -> Index
rotate (x, y, z) = (y, z, x)

orientations :: Index -> [Index]
orientations s =
  [ g n . f3 . f2 $ s
    | n <- [0 .. 3],
      let g n x = iterate (\(a, b, c) -> (a, c, -b)) x !! n,
      f2 <- [id, rotate, rotate . rotate],
      f3 <- [id, \(a, b, c) -> (-a, -b, c)]
  ]

sub :: Index -> Index -> Index
sub (a, b, c) (d, e, f) = (a - d, b - e, c - f)

parseInput :: String -> (Int, [Index])
parseInput s = (n, i)
  where
    s' = lines s
    n = read $ filter isDigit $ head s'
    i = map ((\(x : y : z : _) -> (x, y, z)) . map read . splitOn ",") $ tail s'

overlap :: [Index] -> [Index] -> [[Index]]
overlap ref x = do
  picked <- x
  r <- ref
  let diff = picked `sub` r
  oriented <- transpose $ map orientations x
  let moved = map (`sub` diff) oriented
  guard $ length (ref `intersect` moved) >= 12 
  return moved

day19 :: IO ()
day19 = do
  input <- map parseInput . splitOn "\n\n" <$> readFile "input/test19.txt"
  let input' = map snd input
  -- print $ map (overlap (head input')) (tail input')
  print input'
