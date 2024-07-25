{-# LANGUAGE LambdaCase #-}

module Day11 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (chr, digitToInt, intToDigit, ord)
import Data.List (findIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import MyLib (drawGraph, drawMap)
import Paths_AOC2021

type Index = (Int, Int)

type Octopus = Map Index Int

surround :: [Index]
surround =
  [ (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1)
  ]

inc :: Octopus -> Octopus
inc = Map.map (+ 1)

flashes :: Octopus -> Octopus
flashes m
  | all (< 10) m = m
  | otherwise = flashes (Map.mapWithKey f m)
  where
    f k a
      | a <= 0 = a
      | a > 9 = 0
      | otherwise = a + length (filter (> 9) (mapMaybe ((m Map.!?) . bimap (+ fst k) (+ snd k)) surround))

step = flashes . inc

day11 :: IO ()
day11 = do
  -- input <- drawMap (Just . digitToInt) . lines <$> readFile "input/test11.txt"
  input <- drawMap (Just . digitToInt) . lines <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  let x = iterate step input
  putStrLn
    . ("day11a: " ++)
    . show
    . sum
    . map (length . Map.filter (== 0))
    . take 101
    $ x
  putStrLn
    . ("day11b: " ++)
    . show
    . findIndex (all (== 0))
    $ x
