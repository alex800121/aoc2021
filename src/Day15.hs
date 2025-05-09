{-# LANGUAGE MultiWayIf #-}

module Day15 where

import Control.Monad.ST.Strict (ST, runST)
import Data.Array.MArray qualified as MA
import Data.Array.ST (STUArray)
import Data.Array.Unboxed (Array, inRange)
import Data.Array.Unboxed qualified as A
import Data.Char (digitToInt, intToDigit)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as UM
import MyLib (drawArray, (+&))
import Paths_AOC2021 (getDataDir)
import Debug.Trace

type Index = (Int, Int)

adjacentIndex = [(0, 1), (0, -1), (1, 0), (-1, 0)]

inputParser s = ((width, height), v)
  where
    a :: Array Index Int = A.amap digitToInt $ drawArray $ lines s
    ((mx, my), (nx, ny)) = A.bounds a
    width = nx - mx + 1
    height = ny - my + 1
    v = UV.generate (width * height) ((a A.!) . (`divMod` height))

manhattan (x, y) (z, w) = abs (x - z) + abs (y - w)

type Q s = STUArray s (Int, Int) Int

type L s = UM.STVector s Int

dijkstra factor ((w, h), a) = runST $ do
  q <- MA.newArray ((0, 0), (9, h * 2 + w * 2)) 0
  cache <- UM.replicate (h * w * factor * factor) False
  len <- UM.replicate 10 0
  UM.write len 0 1
  go cache len q 0
  where
    startIndex = (0, 0)
    endIndex = (w * factor - 1, h * factor - 1)
    start = toInt factor startIndex
    end = toInt factor endIndex
    getRisk (x, y) = ((r - 1 + fx + fy) `mod` 9) + 1
      where
        (fx, x') = x `divMod` w
        (fy, y') = y `divMod` h
        r = a UV.! toInt 1 (x', y')
    toInt factor (x, y) = x * h * factor + y
    fromInt factor = (`divMod` (h * factor))
    go :: UM.STVector s Bool -> L s -> Q s -> Int -> ST s Int
    go cache len q risk = do
      counter <- UM.read len riskIndex
      UM.write len riskIndex 0
      f cache len q (pred counter)
      where
        riskIndex = risk `mod` 10
        f cache len q counter | counter < 0 = go cache len q (succ risk)
        f cache len q counter = do
          xy <- MA.readArray q (riskIndex, counter)
          g cache len q xy adjacentIndex
          where
            g cache len q xy [] = f cache len q (pred counter)
            g cache len q xy (y : ys)
              | xy' == endIndex = pure risk'
              |inRange (startIndex, endIndex) xy' = do
                  b <- UM.read cache xyInt'
                  if b then g cache len q xy ys else do
                      counter' <- UM.read len riskIndex'
                      MA.writeArray q (riskIndex', counter') xyInt'
                      UM.write len riskIndex' (counter' + 1)
                      UM.write cache xyInt' True
                      g cache len q xy ys
              | otherwise = g cache len q xy ys
              where
                xy' = fromInt factor xy +& y
                risk' = getRisk xy' + risk
                xyInt' = toInt factor xy'
                riskIndex' = risk' `mod` 10

day15 :: IO ()
day15 = do
  smallMap <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input15.txt"))
  putStrLn 
    . ("day15a: " ++)
    . show
    $ dijkstra 1 smallMap
  putStrLn 
    . ("day15b: " ++)
    . show
    $ dijkstra 5 smallMap
