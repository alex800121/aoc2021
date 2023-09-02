module Day22 where

import Data.List.Split (splitOn, splitOneOf)
import MyLib
import Data.List (uncons, foldl')
import Data.Char (isNumber)
import Data.Maybe (mapMaybe)

type S3 = S (S (S Z))
type V3 = Vec S3
type Range = (Int, Int)
s3 = SS (SS (SS SZ))

parseInput :: String -> (Bool, V3 Range)
parseInput s = (b', v)
  where
    Just (b, s') = uncons $ words s
    b' = b == "on"
    t = map (read @Int) $ filter (any isNumber) $ splitOneOf "=,." $ head s'
    v = toVec s3 [(head t, t !! 1 + 1), (t !! 2, t !! 3 + 1), (t !! 4, t !! 5 + 1)]

readIns :: [(Bool, V3 Range)] -> [V3 Range]
readIns = foldl' f []
  where
    f acc (True, v) = jointEucVecs' acc v
    f acc (False, v) = subtractEucVecs' v acc

calcOn :: V3 Range -> Int
calcOn = foldl' (\acc (x, y) -> acc * (y - x)) 1

day22 :: IO ()
day22 = do
  input <- map parseInput . lines <$> readFile "input/input22.txt"
  let ans = readIns input
  putStrLn $ ("day22a: " ++) $ show $ sum $ mapMaybe (fmap calcOn . overlapEucVec (toVec s3 (replicate 3 (-50, 51)))) ans
  putStrLn $ ("day22b: " ++) $ show $ sum $ map calcOn ans
