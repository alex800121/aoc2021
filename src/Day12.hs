module Day12 where

import Data.Char (isLower)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (findIndex, foldl', nub, sort)
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as Map
import Debug.Trace
import Paths_AOC2021

inputParser s = (small + 2, im)
  where
    ss = map (splitOn "-") $ lines s
    ss' =
      nub
        . sort
        . filter (`notElem` ["start", "end"])
        $ concat ss
    is =
      Map.fromList
        . (`zip` [0 ..])
        $ ["start", "end"] <> ss'
    Just small = findIndex (isLower . head) ss'
    im = foldl' f IM.empty ss
    f acc [x, y] = IM.insertWith (<>) ix [iy] $ IM.insertWith (<>) iy [ix] acc
      where
        ix = is Map.! x
        iy = is Map.! y

dfs im small end !visitedTwice (!start, !visited)
  | start == end = 1
  | visitedTwice && start >= small && start `IS.member` visited = 0
  | otherwise = foldl' (\acc x -> acc + dfs im small end (visitedTwice || start >= small && start `IS.member` visited) x) 0 choices
  where
    choices =
      [(start', IS.insert start visited) | start' <- im IM.! start, start' /= 0]

day12 :: IO ()
day12 = do
  (small, im) <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  putStrLn
    . ("day12a: " ++)
    . show
    $ dfs im small 1 True (0, IS.empty)
  putStrLn
    . ("day12b: " ++)
    . show
    $ dfs im small 1 False (0, IS.empty)
