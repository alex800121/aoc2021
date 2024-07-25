module Day6 where

import Data.IntMap qualified as IntMap
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Paths_AOC2021

type Fish = IntMap.IntMap Int

nextGen :: Fish -> Fish
nextGen f = IntMap.insertWith (+) 6 (f IntMap.! 0) $ IntMap.mapKeys ((`mod` 9) . subtract 1) f

day6 :: IO ()
day6 = do
  input <- IntMap.unionWith (+) (IntMap.fromList [(x, 0) | x <- [0 .. 8]]) . IntMap.fromList . map (\x -> (head x, length x)) . group . sort . map (read @Int) . splitOn "," <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  putStrLn $ ("day6a: " ++) $ show $ sum $ IntMap.elems $ (!! 80) $ iterate nextGen input
  putStrLn $ ("day6b: " ++) $ show $ sum $ IntMap.elems $ (!! 256) $ iterate nextGen input
