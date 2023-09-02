{-# LANGUAGE LambdaCase #-}
module Day15 where

import Data.Char (digitToInt, intToDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Debug.Trace
import MyLib (drawGraph, drawMap, sqrtCeiling)
import Data.Bifunctor (bimap)

type Index = (Int, Int)

type Risk = Int

type Hue = Int

type Chitons = Map Index Int

type Queue = MinPQueue Risk Index
type Queue' = MinPQueue Hue (Index, Risk)

type Cache = Set Index

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

dijkstra :: Chitons -> Index -> Queue -> Cache -> Int
dijkstra chitons end q cache
  | start == end = nextRisk
  | start `Set.member` cache = dijkstra chitons end q' cache
  | otherwise = dijkstra chitons end q'' cache'
  where
    ((risk, start), q') = Q.deleteFindMin q
    nextRisk = risk + chitons Map.! start
    nexts =
      Q.fromList $
        mapMaybe
          ( \(x, y) ->
              let next = (fst start + x, snd start + y)
               in if Map.member next chitons && Set.notMember next cache
                    then Just (nextRisk, next)
                    else Nothing
          )
          adjacent
    q'' = Q.union nexts q'
    cache' = Set.insert start cache

-- aStar :: Chitons -> Index -> Queue' -> Cache -> Int
-- aStar chitons end q cache
--   | start == end = nextRisk
--   | start `Set.member` cache = aStar chitons end q' cache
--   | otherwise = aStar chitons end q'' cache'
--   where
--     ((hue, (start, risk)), q') = Q.deleteFindMin q
--     nextRisk = risk + chitons Map.! start
--     nextHue = nextRisk + sqrtCeiling ((fst end - fst start) ^ 2 + (snd end - snd start) ^ 2)
--     nexts =
--       Q.fromList $
--         mapMaybe
--           ( \(x, y) ->
--               let next = (fst start + x, snd start + y)
--                in if Map.member next chitons && Set.notMember next cache
--                     then Just (nextHue, (next, nextRisk))
--                     else Nothing
--           )
--           adjacent
--     q'' = Q.union nexts q'
--     cache' = Set.insert start cache

day15 :: IO ()
day15 = do
  smallMap <- drawMap (Just . digitToInt) . lines <$> readFile "input/input15.txt"
  -- smallMap <- drawMap (Just . digitToInt) . lines <$> readFile "input/test15.txt"
  let keys = Map.keys smallMap
      start = minimum keys
      end = maximum keys
      width = fst end - fst start + 1
      height = snd end - snd start + 1
      bigMap = Map.unions . Set.map (\(x, y) -> let added = 10 - x - y in
        Map.mapKeys (bimap (+ (x * width)) (+ (y * height))) $ Map.map ((+ 1) . (`mod` 9) . subtract added) smallMap) $ Set.fromList [(x, y) | x <- [0..4], y <- [0..4]]
      keys' = Map.keys bigMap
      start' = minimum keys'
      end' = maximum keys'
      initRisk' = negate (bigMap Map.! start')
      initHue' = initRisk' + ((fst end' - fst start') + (snd end' - snd start'))
  putStrLn $ ("day15a: " ++) $ show $ dijkstra smallMap end (Q.singleton (negate (smallMap Map.! start)) start) Set.empty
  putStrLn $ ("day15a: " ++) $ show $ dijkstra bigMap end' (Q.singleton (negate (bigMap Map.! start')) start') Set.empty
