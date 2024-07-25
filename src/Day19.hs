module Day19 where

import Control.Applicative (Alternative (empty))
import Control.Monad (guard)
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import Data.List (insert, intersect, nub, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (trace)
import Paths_AOC2021

type Index = (Int, Int, Int)

rotate :: Index -> Index
rotate (x, y, z) = (y, z, x)

orientations :: Set Index -> [Set Index]
orientations s =
  [ Set.map (g n . f3 . f2) s
    | n <- [0 .. 3],
      let g n x = iterate (\(a, b, c) -> (a, c, -b)) x !! n,
      f2 <- [id, rotate, rotate . rotate],
      f3 <- [id, \(a, b, c) -> (-a, -b, c)]
  ]

sub :: Index -> Index -> Index
sub (a, b, c) (d, e, f) = (a - d, b - e, c - f)

distances :: Set Index -> Set Int
distances d = Set.fromList $ do
  let d' = Set.toList d
  x <- d'
  y <- d'
  guard $ x > y
  let (i, j, k) = sub x y
  return $ i ^ 2 + j ^ 2 + k ^ 2

parseInput :: String -> Set Index
parseInput s = Set.fromList i
  where
    s' = lines s
    i = map ((\(x : y : z : _) -> (x, y, z)) . map read . splitOn ",") $ tail s'

overlap :: (Set Index, Set Int) -> (Set Index, Set Int) -> Either (Set Index, Set Int) (Index, (Set Index, Set Int))
overlap (ref, rfp) (x, xfp) | length (Set.intersection rfp xfp) < 66 = Left (x, xfp)
overlap (ref, rfp) (x, xfp) = maybe (Left (x, xfp)) Right $ listToMaybe $ do
  r <- Set.toList ref
  oriented <- orientations x
  picked <- Set.toList oriented
  let diff = picked `sub` r
  let moved = Set.map (`sub` diff) oriented
  guard $ length (Set.intersection ref moved) >= 12
  -- return (ref `intersect` moved)
  return ((0, 0, 0) `sub` diff, (moved, xfp))

day19a :: [(Index, (Set Index, Set Int))] -> [(Set Index, Set Int)] -> ([Index], Set Index) -> ([Index], Set Index)
-- day19a x y acc | trace (show (length x, length y, length $ snd acc)) False = undefined
day19a [] (y : ys) acc = day19a [((0, 0, 0), y)] ys acc
day19a xs [] acc = bimap (map fst xs <>) (Set.unions (map (fst . snd) xs) <>) acc
day19a (x : xs) ys acc = day19a (xs <> ysRight) ysLeft (bimap (insert (fst x)) (fst (snd x) <>) acc)
  where
    (ysLeft, ysRight) = partitionEithers (map (overlap (snd x)) ys)

day19b :: [Index] -> Int
day19b s = go s 0
  where
    go [] m = m
    go (x : xs) m = go xs $ maximum $ m : map ((\(x, y, z) -> abs x + abs y + abs z) . sub x) xs

day19 :: IO ()
day19 = do
  input <- map parseInput . splitOn "\n\n" <$> (getDataDir >>= readFile . (++ "/input/input19.txt"))
  -- input <- map parseInput . splitOn "\n\n" <$> readFile "input/test19.txt"
  let input' = zip input (map distances input)
      ans = day19a [] input' ([], Set.empty)
  putStrLn $ ("day19a: " ++) $ show $ length $ snd ans
  putStrLn $ ("day19b: " ++) $ show $ day19b $ fst ans
