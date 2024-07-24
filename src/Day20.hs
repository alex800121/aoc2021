{-# LANGUAGE TupleSections #-}

module Day20 where

import Paths_AOC2021
import Control.Monad.Trans.State.Strict
import Paths_AOC2021
import Data.Array.IArray (Array, elems, listArray, array, (!), IArray (bounds))
import Paths_AOC2021
import Data.List.Split (splitOn)
import Paths_AOC2021
import Data.Map (Map)
import Paths_AOC2021
import qualified Data.Map as Map
import Paths_AOC2021
import Data.Maybe (fromMaybe)
import Paths_AOC2021
import MyLib (drawMap)
import Paths_AOC2021
import Control.Monad (forM)
import Paths_AOC2021
import Data.Bifunctor (bimap)
import Paths_AOC2021
import Data.List (foldl')
import Paths_AOC2021
import Data.Char (isSpace)

type Rule = Array Int Bool

type Gen = Int

type Index = (Int, Int)

type Bound = (Index, Index)

type Mem = Map (Gen, Index) Bool

type Trench = (Bool, Array Index Bool)

nextTrench :: Rule -> Trench -> Trench
nextTrench rule (def, trench) = (not def, trench')
  where
    ((minX, minY), (maxX, maxY)) = bounds trench
    newBound@((minX', minY'), (maxX', maxY')) = ((minX - 1, minY - 1), (maxX + 1, maxY + 1))
    f x y
      | x < minX || x > maxX || y < minY || y > maxY = def
      | otherwise = trench ! (x, y)
    g x y = rule ! foldl' (\acc z -> (acc * 2) + if z then 1 else 0) 0 (map (uncurry f . bimap (+ x) (+ y)) box)
    trench' = array newBound [((x, y), g x y) | x <- [minX' .. maxX'], y <- [minY' .. maxY']]

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

printArray :: (a -> b) -> Array Index a -> [[b]]
printArray f arr = g minX minY id
  where
    ((minX, minY), (maxX, maxY)) = bounds arr
    g x y acc
      | y > maxY = []
      | x > maxX = acc [] : g minX (y + 1) id
      | otherwise = g (x + 1) y (acc . (f (arr ! (x, y)) :))

day20 :: IO ()
day20 = do
  input <- splitOn "\n\n" <$> readFile "input/input20.txt"
  let rule = listArray (0, 511) $ map (== '#') $ filter (not . isSpace) $ head input :: Array Int Bool
      mem = Map.mapKeys (0,) $ drawMap (Just . (== '#')) $ lines (input !! 1)
      (minX, minY) = minimum $ map snd $ Map.keys mem
      (maxX, maxY) = maximum $ map snd $ Map.keys mem
      arr = array ((minX, minY), (maxX, maxY)) $ Map.toList $ Map.mapKeys snd mem :: Array Index Bool
      rangeN n = [(n, (x, y)) | x <- [minX - n .. maxX + n], y <- [minY - n .. maxY + n]]
      rangeA = rangeN 2
      rangeB = rangeN 50
  putStrLn $ ("day20a: " ++) $ show $ length $ filter id $ elems $ snd $ (!! 2) $ iterate (nextTrench rule) (False, arr)
  putStrLn $ ("day20b: " ++) $ show $ length $ filter id $ elems $ snd $ (!! 50) $ iterate (nextTrench rule) (False, arr)
