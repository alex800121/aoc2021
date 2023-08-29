{-# LANGUAGE TupleSections #-}

module Day20 where

import Control.Monad.Trans.State.Strict
import Data.Array.IArray (Array, listArray, (!))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import MyLib (drawMap)
import Control.Monad (forM)
import Data.Bifunctor (bimap)
import Data.List (foldl')
import Data.Char (isSpace)

type Rule = Array Int Bool

type Gen = Int

type Index = (Int, Int)

type Bound = (Index, Index)

type Mem = Map (Gen, Index) Bool

type Trench = (Bool, Array Index Bool)

nextTrench :: Trench -> Trench
nextTrench (def, trench) = undefined

box :: [Index]
box = [(x, y) | y <- [-1 .. 1], x <- [-1 .. 1]]

getGenIndex :: Bound -> Rule -> (Gen, Index) -> State Mem Bool
getGenIndex ((minX, maxX), (minY, maxY)) rule (gen, index@(x, y))
  | x < minX - gen || x > maxX + gen || y < minY - gen || y > maxY + gen = return (odd gen)
getGenIndex bound rule (gen, index) =
  get >>= \mem -> case mem Map.!? (gen, index) of
    Just x -> return x
    Nothing | gen <= 0 -> return False
    Nothing -> do
      b <- forM box $ \(x, y) -> getGenIndex bound rule (gen - 1, bimap (+ x) (+ y) index)
      let b' = foldl' (\acc x -> (acc * 2) + if x then 1 else 0) 0 b
          ans = rule ! b'
      modify (Map.insert (gen, index) ans)
      return ans

day20 :: IO ()
day20 = do
  input <- splitOn "\n\n" <$> readFile "input/input20.txt"
  -- input <- splitOn "\n\n" <$> readFile "input/test20.txt"
  let rule = listArray (0, 511) $ map (== '#') $ filter (not . isSpace) $ head input
      mem = Map.mapKeys (0,) $ drawMap (Just . (== '#')) $ lines (input !! 1)
      (minX, minY) = minimum $ map snd $ Map.keys mem
      (maxX, maxY) = maximum $ map snd $ Map.keys mem
      rangeN n = [(n, (x, y)) | x <- [minX - n .. maxX + n], y <- [minY - n .. maxY + n]]
      rangeA = rangeN 2
      rangeB = rangeN 50
  putStrLn $ ("day20a: " ++) $ show $ length $ filter id $ (`evalState` mem) $ traverse (getGenIndex ((minX, maxX), (minY, maxY)) rule) rangeA
  putStrLn $ ("day20b: " ++) $ show $ length $ filter id $ (`evalState` mem) $ traverse (getGenIndex ((minX, maxX), (minY, maxY)) rule) rangeB
