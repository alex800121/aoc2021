{-# LANGUAGE LambdaCase #-}

module Day9 where

import Data.Bifunctor (bimap)
import Data.Char (chr, digitToInt, intToDigit, ord)
import Data.Either (isLeft, lefts)
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib
import Paths_AOC2021

type Index = (Int, Int)

type Lava = Map Index (Either Int Int)

type Basin = Set Index

adjacent :: [Index]
adjacent =
  [ (0, 1),
    (0, -1),
    (1, 0),
    (-1, 0)
  ]

drawLava :: Lava -> String
drawLava =
  unlines
    . drawGraph
      ( \case
          Nothing -> ' '
          Just (Right a) -> intToDigit a
          Just (Left a) -> chr (a + ord 'a')
      )

getBasins :: Lava -> Basin -> Basin -> Set Basin -> Set Basin
getBasins lava currentBasin currentAcc acc
  | Set.null currentBasin && Set.null nextSearch = acc'
  | Set.null currentBasin = getBasins lava (Set.singleton $ Set.findMin nextSearch) Set.empty acc'
  | otherwise = getBasins lava' nextBasin currentAcc' acc
  where
    nextSearch = Map.keysSet $ Map.filter isLeft lava'
    lava' =
      Map.unionWith
        ( \x _ -> case x of
            Left x -> Right x
            y -> y
        )
        lava
        (Map.fromSet (const undefined) currentBasin)
    nextBasin = Set.filter (isLeft . fromMaybe (Right 0) . (lava' Map.!?)) $ Set.unions $ map (\(x, y) -> Set.map (bimap (+ x) (+ y)) currentBasin) adjacent
    currentAcc' = Set.union currentBasin currentAcc
    acc' = Set.insert currentAcc acc

searchLowPoints :: Lava -> Lava
searchLowPoints lava
  -- \| trace (drawLava lava) False = undefined
  | lava == lava' = lava
  | otherwise = searchLowPoints lava'
  where
    lava' = Map.mapWithKey f lava
    g a b = case b of
      Right b -> a >= b
      Left b -> a > b
    f k a = case a of
      Left a ->
        let ks = mapMaybe ((lava Map.!?) . bimap (+ fst k) (+ snd k)) adjacent
         in if any (g a) ks then Right a else Left a
      b -> b

day9 :: IO ()
day9 = do
  -- input <- drawMap (Just . Left @Int @Int . digitToInt) . lines <$> readFile "input/test9.txt"
  input <- drawMap (Just . digitToInt) . lines <$> (getDataDir >>= readFile . (++ "/input/input9.txt"))
  putStrLn
    . ("day9a: " ++)
    . show
    . sum
    . map (either (+ 1) (const 0))
    . Map.elems
    . searchLowPoints
    . Map.map Left
    $ input
  putStrLn
    . ("day9b: " ++)
    . show
    . product
    . take 3
    . reverse
    . sort
    . map Set.size
    . Set.toList
    $ getBasins (Map.map (\case 9 -> Right 0; x -> Left x) input) Set.empty Set.empty Set.empty
