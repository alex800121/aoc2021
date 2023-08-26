{-# LANGUAGE LambdaCase #-}

module Day13 where

import Data.List (foldl', scanl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (drawGraph, drawMap)

type Index = (Int, Int)

type Paper = Set Index

data FoldInstruction = X Int | Y Int deriving (Show, Eq, Ord)

parseInput :: String -> (Paper, [FoldInstruction])
parseInput s = (paper, foldins)
  where
    (x : y : _) = splitOn "\n\n" s
    paper = Set.fromList . map ((\(x : y : _) -> (read x, read y)) . splitOn ",") $ lines x
    foldins = map (f . splitOn "=") (lines y)
    f ("fold along x" : x : _) = X (read x)
    f ("fold along y" : y : _) = Y (read y)
    f a = error (show a)

foldPaper :: Paper -> FoldInstruction -> Paper
foldPaper p (X i) = Set.map (\(x, y) -> (if x > i then (2 * i) - x else x, y)) p
foldPaper p (Y i) = Set.map (\(x, y) -> (x, if y > i then (2 * i) - y else y)) p

day13 :: IO ()
day13 = do
  input <- readFile "input/input13.txt"
  let folded = uncurry (scanl' foldPaper) (parseInput input)
  putStrLn $ ("day13b: " ++) $ show $ length $ folded !! 1
  putStrLn "day13b:"
  mapM_ putStrLn $ drawGraph (\case Nothing -> ' '; Just _ -> '#') $ Map.fromSet (const ()) $ last folded
