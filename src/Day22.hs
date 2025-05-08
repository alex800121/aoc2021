module Day22 where

import Control.Parallel (par)
import Data.Char (isNumber)
import Data.Function (on)
import Data.List (foldl', sortBy, uncons)
import Data.Maybe (mapMaybe)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Paths_AOC2021
import Data.List.Split (splitOneOf)

type Range = (Int, Int)

type V3 a = (a, a, a)

overlapEucVec x@((a0, a1), (b0, b1), (c0, c1)) y@((d0, d1), (e0, e1), (f0, f1))
  | g1 > g0 && h1 > h0 && i1 > i0 = Just ans
  | otherwise = Nothing
  where
    g@(g0, g1) = (max a0 d0, min a1 d1)
    h@(h0, h1) = (max b0 e0, min b1 e1)
    i@(i0, i1) = (max c0 f0, min c1 f1)
    ans = (g, h, i)

parseInput :: String -> (Bool, V3 Range)
parseInput s = (b', v)
  where
    Just (b, s') = uncons $ words s
    b' = b == "on"
    t = map (read @Int) $ filter (any isNumber) $ splitOneOf "=,." $ head s'
    v = ((head t, t !! 1 + 1), (t !! 2, t !! 3 + 1), (t !! 4, t !! 5 + 1))

readIns :: [(Bool, V3 Range)] -> (MultiSet (V3 Range), MultiSet (V3 Range))
readIns = foldl' f (MS.empty, MS.empty)
  where
    f (on, off) (b, v) = (on', off')
      where
        on' = (if b then MS.insert v else id) (MS.mapMaybe (overlapEucVec v) off <> on)
        off' = par on' MS.mapMaybe (overlapEucVec v) on <> off

calcOn :: V3 Range -> Int
calcOn ((x0, x1), (y0, y1), (z0, z1)) = (x1 - x0) * (y1 - y0) * (z1 - z0)

day22 :: IO ()
day22 = do
  input <- map parseInput . lines <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  let ans = readIns input
      area = ((-50, 50), (-50, 50), (-50, 50))
      ansA = par ansB uncurry ((-) `on` (sum . MS.mapMaybe (fmap calcOn . overlapEucVec area))) ans
      ansB = uncurry ((-) `on` (sum . MS.map calcOn)) ans
  putStrLn
    . ("day22a: " ++)
    . show
    $ ansA
  putStrLn
    . ("day22b: " ++)
    . show
    $ ansB
