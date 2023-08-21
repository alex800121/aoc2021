module Day15 where

import Data.Char (digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import MyLib (drawGraph, drawMap, sqrtCeiling)

type Index = (Int, Int)

type Risk = Int

type Weight = Int

type Candidate = (Risk, Index, Set Index)

type Candidate' = (Weight, Risk, Index, Set Index)

type Chitons = Map Index Int

type Queue = Set Candidate

type Queue' = Set Candidate'

type Cache = Set (Index, Set Index)

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

dijkstra :: Chitons -> Index -> Queue -> Cache -> Int
dijkstra chitons end q cache
  -- \| trace (show (length q)) False = undefined
  | start == end = nextRisk
  | (start, path) `Set.member` cache = dijkstra chitons end q' cache
  | otherwise = dijkstra chitons end q'' cache'
  where
    (current@(risk, start, path), q') = Set.deleteFindMin q
    nextRisk = risk + chitons Map.! start
    nexts =
      Set.fromList $
        mapMaybe
          ( \(x, y) ->
              let next = (fst start + x, snd start + y)
               in if Map.member next chitons && Set.notMember next path
                    then Just (nextRisk, next, Set.insert next path)
                    else Nothing
          )
          adjacent
    q'' = Set.union nexts q'
    cache' = Set.insert (start, path) cache

-- aStar :: Chitons -> Index -> Queue' -> Cache -> Int
-- aStar chitons end q cache
--   | trace (show (length q, start)) False = undefined
--   | start == end = nextRisk
--   | (start, path) `Map.member` cache = aStar chitons end q' cache
--   | otherwise = aStar chitons end q'' cache'
--   where
--     (current@(weight, risk, start, path), q') = Set.deleteFindMin q
--     nextRisk = risk + chitons Map.! start
--     nextWeight = nextRisk + (2 * sqrtCeiling (abs (fst end - fst start) ^ 2 + abs (snd end - snd start) ^ 2))
--     nexts =
--       Set.fromList $
--         mapMaybe
--           ( \(x, y) ->
--               let next = (fst start + x, snd start + y)
--                in if Map.member next chitons && Set.notMember next path
--                     then Just (nextWeight, nextRisk, next, Set.insert next path)
--                     else Nothing
--           )
--           adjacent
--     q'' = Set.union nexts q'
--     cache' = Map.insertWith min (start, path) risk cache

day15 :: IO ()
day15 = do
  input <- drawMap (Just . digitToInt) . lines <$> readFile "input/input15.txt"
  -- input <- drawMap (Just . digitToInt) . lines <$> readFile "input/test15.txt"
  let keys = Map.keys input
      start = minimum keys
      end = maximum keys
      initRisk = negate (input Map.! start)
      initWeight = initRisk + abs (fst end - fst start) + abs (snd end - snd start)
  print end
  -- print $ dijkstra input end (Set.singleton (negate (input Map.! start), start, Set.empty)) Set.empty

-- print $ aStar input end (Set.singleton (initWeight, initRisk, start, Set.empty)) Map.empty
