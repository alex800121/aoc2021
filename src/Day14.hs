module Day14 where

import Data.Char (isSpace)
import Data.List.Split (divvy, splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MultiSet
import Paths_AOC2021

type Formula = Map String (MultiSet String)

type Polymer = MultiSet String

type Molecules = MultiSet Char

parseInput :: String -> (Polymer, Formula, Char, Char)
parseInput s = (polymer, formula, head x, last x)
  where
    (x : y : _) = splitOn "\n\n" s
    polymer = MultiSet.fromList $ divvy 2 1 x
    formula = Map.fromList $ map f $ lines y
    f n = (input, output)
      where
        (a : b : _) = splitOn " -> " n
        input = a
        output = MultiSet.fromList [head a : b, head b : tail a]

step :: Formula -> Polymer -> Polymer
step f = MultiSet.unionsMap (f Map.!)

calc :: Polymer -> Molecules
calc = MultiSet.unionsMap MultiSet.fromList

ansA :: Molecules -> Int
ansA m = (maxM - minM) `div` 2
  where
    m' = map snd $ MultiSet.toOccurList m
    maxM = maximum m'
    minM = minimum m'

day14 :: IO ()
day14 = do
  input <- (getDataDir >>= readFile . (++ "/input/input14.txt"))
  let (polymer, formula, start, end) = parseInput input
  putStrLn
    . ("day14a: " ++)
    . show
    . ansA
    . MultiSet.insert end
    . MultiSet.insert start
    . calc
    . (!! 10)
    $ iterate (step formula) polymer
  putStrLn
    . ("day14b: " ++)
    . show
    . ansA
    . MultiSet.insert end
    . MultiSet.insert start
    . calc
    . (!! 40)
    $ iterate (step formula) polymer
