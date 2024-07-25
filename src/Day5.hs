module Day5 where

import Paths_AOC2021
import qualified Data.Map as Map
import Paths_AOC2021
import qualified Data.List.Split as Split

type Index = (Int, Int)
type Ends = (Index, Index)
type Line = Map.Map Index Int

drawLine :: Ends -> Line
drawLine (start, end) = go acc start end
  where
    x = signum (fst end - fst start)
    y = signum (snd end - snd start)
    acc = Map.empty
    go acc start end
      | start == end = Map.insertWith (+) end 1 acc
      | otherwise = go (Map.insertWith (+) start 1 acc) (fst start + x, snd start + y) end

day5 :: IO ()
day5 = do
  input <- map ((\(x : y : _) -> (x, y)) . map ((\(x : y : _) -> (x, y)) . map (read @Int) . Split.splitOn ",") . Split.splitOn " -> ") . lines <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let lineA = length $ Map.filter (> 1) $ Map.unionsWith (+) $ map drawLine $ filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) input
      lineB = length $ Map.filter (> 1) $ Map.unionsWith (+) $ map drawLine input
  putStrLn $ ("day5a: " ++) $ show lineA
  putStrLn $ ("day5b: " ++) $ show lineB
