module Day19 where

import Control.Applicative (Alternative (empty))
import Data.List (intersect, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Char (isDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (guard)
import Debug.Trace (trace)
import Data.Either (partitionEithers)

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

overlap :: [Index] -> [Index] -> Either [Index] [Index]
overlap ref x = maybe (Left x) Right $ listToMaybe $ do
  r <- ref
  oriented <- traverse orientations x
  picked <- oriented
  let diff = picked `sub` r
  let moved = map (`sub` diff) oriented
  -- return (ref `intersect` moved)
  guard $ length (ref `intersect` moved) >= 12 
  return moved

day19a :: [[Index]] -> [[Index]] -> Set Index -> Set Index
day19a x y acc | trace (show (length x, length y, length acc)) False = undefined
day19a [] (y : ys) acc = day19a [y] ys acc
day19a xs [] acc = acc <> Set.fromList (concat xs)
day19a (x : xs) ys acc = day19a (xs <> ysRight) ysLeft (acc <> Set.fromList x)
  where
    (ysLeft, ysRight) = partitionEithers (map (overlap x) ys)

day19 :: IO ()
day19 = do
  input <- map parseInput . splitOn "\n\n" <$> readFile "input/input19.txt"
  -- input <- map parseInput . splitOn "\n\n" <$> readFile "input/test19.txt"
  let input' = map snd input
  print $ traverse orientations [(1, 2, 3), (4, 5, 6)]
  -- print $ length $ day19a [] input' Set.empty
