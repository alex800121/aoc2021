module Day19 where

import Control.Applicative (Alternative (empty))
import Control.Monad (guard, when)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (foldl', insert, intersect, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace, traceM, traceShow)
import Paths_AOC2021

type Index = (Int, Int, Int)

type DistMap = IntMap [Index]

rotate :: Index -> Index
rotate (x, y, z) = (y, z, x)

orientations :: [Index -> Index]
orientations =
  [ g n . f3 . f2
    | n <- [0 .. 3],
      let g n x = iterate (\(a, b, c) -> (a, c, -b)) x !! n,
      f2 <- [id, rotate, rotate . rotate],
      f3 <- [id, \(a, b, c) -> (-a, -b, c)]
  ]

(-&) :: Index -> Index -> Index
(a, b, c) -& (d, e, f) = (a - d, b - e, c - f)

distances :: Set Index -> DistMap
distances d = IM.fromListWith (<>) $ do
  let d' = Set.toList d
  x <- d'
  y <- d'
  guard $ x > y
  let i = x -& y
  return (distance i, [x, y])

distance (i, j, k) = i ^ 2 + j ^ 2 + k ^ 2

parseInput :: String -> Set Index
parseInput s = Set.fromList i
  where
    s' = lines s
    i = map ((\(x : y : z : _) -> (x, y, z)) . map read . splitOn ",") $ tail s'

overlap :: (Set Index, DistMap) -> (Set Index, DistMap) -> Either (Set Index, DistMap) (Index, (Set Index, DistMap))
overlap (ref, rfp) (xef, xfp)
  | length r0 < 12 || length x0 < 12 = Left (xef, xfp)
  | otherwise = maybe (Left (xef, xfp)) Right $ listToMaybe $ do
      r <- Set.toList r0
      f <- orientations
      let oriented = Set.map f x0
      picked <- Set.toList oriented
      let diff = picked -& r
      let moved = Set.map (-& diff) oriented
      let move = (-& diff) . f
      guard $ length (Set.intersection r0 moved) >= 12
      return ((0, 0, 0) -& diff, (Set.map move xef, IM.map (map move) xfp))
  where
    rxfp = IM.intersectionWith (,) rfp xfp
    (r0, x0) = IM.foldl' (\(r, x) (a, b) -> (r <> Set.fromList a, x <> Set.fromList b)) (Set.empty, Set.empty) rxfp

day19a :: [(Index, (Set Index, DistMap))] -> [(Set Index, DistMap)] -> ([Index], Set Index) -> ([Index], Set Index)
day19a [] (y : ys) acc = day19a [((0, 0, 0), y)] ys acc
day19a xs [] acc = bimap (map fst xs <>) (Set.unions (map (fst . snd) xs) <>) acc
day19a (x : xs) ys acc = day19a (xs <> ysRight) ysLeft (bimap (insert (fst x)) (fst (snd x) <>) acc)
  where
    (ysLeft, ysRight) = partitionEithers (map (overlap (snd x)) ys)

day19b :: [Index] -> Int
day19b s = go s 0
  where
    go [] m = m
    go (x : xs) m = go xs $ maximum $ m : map ((\(x, y, z) -> abs x + abs y + abs z) . (-&) x) xs

day19 :: IO ()
day19 = do
  input <- map parseInput . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  let input' = zip input (map distances input)
      ans = day19a [] input' ([], Set.empty)
  putStrLn $ ("day19a: " ++) $ show $ length $ snd ans
  putStrLn $ ("day19b: " ++) $ show $ day19b $ fst ans
