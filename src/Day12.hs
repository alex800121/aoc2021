{-# LANGUAGE TupleSections #-}
module Day12 where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (isUpper)
import Debug.Trace

type Cave = Map String (Set String)
type NotVisited = MultiSet (String, Set String, MultiSet String, MultiSet String)
type Paths = NotVisited

walkTill :: String -> Cave -> NotVisited -> Paths -> Paths
walkTill end cave currentPath cache
  | MultiSet.null currentPath = cache
  | otherwise = walkTill end cave nextPath cache'
  where
    (ended, currentPath') = MultiSet.partition ((== end) . (\(x, _, _, _) -> x)) currentPath
    cache' = MultiSet.union cache ended
    f (start, smallCaves, path, visited)
      | isUpper (head start) || start == "start" || start == "end" = paths
      | start `Set.member` smallCaves = paths1
      | otherwise = paths2
      where
        path' = if isUpper (head start) then path else MultiSet.delete start path
        nexts = Set.filter ((> 0) . (`MultiSet.occur` path)) $ cave Map.! start
        visited' = MultiSet.insert start visited
        paths = MultiSet.fromSet $ Set.map (, smallCaves, path', visited') nexts
        paths1 = MultiSet.fromSet $ Set.map (, smallCaves, MultiSet.fromSet $ MultiSet.toSet path', visited') $ Set.filter (`Set.notMember` smallCaves) nexts
        paths2= MultiSet.fromSet $ Set.map (, Set.insert start smallCaves, path', visited') $ Set.filter (`Set.notMember` smallCaves) nexts
    nextPath = MultiSet.unionsMap f currentPath'

day12 :: IO ()
day12 = do
  input <- Map.map Set.fromList . Map.unionsWith (<>) . map ((\(x : y : _) -> Map.fromList [(x, [y]), (y, [x])]) . splitOn "-") . lines <$> readFile "input/test12.txt"
  -- input <- Map.unionsWith (<>) . map ((\(x : y : _) -> Map.fromList [(x, [y]), (y, [x])]) . splitOn "-") . lines <$> readFile "input/input12.txt"
  let notVisitedA = MultiSet.fromSet $ Map.keysSet input
      notVisitedB = MultiSet.fold (\x acc -> case x of
        x | x == "start" || x == "end" || isUpper (head x) -> acc
        x -> MultiSet.insert x acc) notVisitedA notVisitedA
  mapM_ print $ MultiSet.toOccurList $ walkTill "end" input (MultiSet.singleton ("start", Set.empty, notVisitedB, MultiSet.empty)) MultiSet.empty
  print $ MultiSet.size $ walkTill "end" input (MultiSet.singleton ("start", Set.empty, notVisitedB, MultiSet.empty)) MultiSet.empty
