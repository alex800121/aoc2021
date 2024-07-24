{-# LANGUAGE LambdaCase #-}

module Day23 where

import Paths_AOC2021
import Data.Bifunctor (bimap)
import Paths_AOC2021
import Data.List
import Paths_AOC2021
import Data.Map (Map)
import Paths_AOC2021
import qualified Data.Map as Map
import Paths_AOC2021
import Data.Maybe (isJust, isNothing, mapMaybe)
import Paths_AOC2021
import Data.PQueue.Prio.Min (MinPQueue)
import Paths_AOC2021
import qualified Data.PQueue.Prio.Min as Q
import Paths_AOC2021
import Data.Set (Set)
import Paths_AOC2021
import qualified Data.Set as Set
import Paths_AOC2021
import Debug.Trace
import Paths_AOC2021
import MyLib (drawGraph, drawMapWithKey)

type Index = (Int, Int)

type Risk = Int

type Heu = Int

data Pods = A | B | C | D deriving (Show, Eq, Ord, Read, Enum)

data Space
  = Corridor {getPod :: Maybe Pods}
  | Dest {dest :: Pods, getPod :: Maybe Pods}
  | Space {getPod :: Maybe Pods}
  deriving (Show, Eq, Ord)

finished :: Map Index Space -> Bool
finished =
  all
    ( \case
        Dest x y -> Just x == y
        x -> True
    )
    . Map.elems

dijkstra :: Set (Map Index Space) -> MinPQueue Risk (Map Index Space) -> Maybe Risk
dijkstra acc q = case Q.minViewWithKey q of
  Nothing -> Nothing
  Just ((r, a), q') | a `Set.member` acc -> dijkstra acc q'
  Just ((r, a), q') ->
    if finished a
      then Just r
      else
        let q'' = Q.union q' $ Q.unions $ map (candidates (r, a)) $ Map.keys a
            acc' = Set.insert a acc
         in dijkstra acc' q''

candidates :: (Risk, Map Index Space) -> Index -> MinPQueue Risk (Map Index Space)
candidates (r, m) i = case m Map.!? i of
  Just (Corridor (Just p)) ->
    foldl'
      ( \b (r, k) -> case m Map.!? k of
          Just (Dest y Nothing)
            | p == y && isDest' p (fmap (+ 1) k) ->
                Q.insert r (Map.insert i (Corridor Nothing) $ Map.insert k (Dest y (Just p)) m) b
          _ -> b
      )
      Q.empty
      $ flooded (risk p)
  Just (Dest p (Just p'))
    | any cond [fmap (+ f) i | f <- [-3 .. 3]] ->
        fst
          $ foldl'
            ( \(b, s) (r, a) -> case m Map.!? a of
                Just (Corridor Nothing)
                  | p' `Set.notMember` s ->
                      ( Q.insert
                          r
                          ( Map.insert i (Dest p Nothing) $
                              Map.insert a (Corridor (Just p')) m
                          )
                          b,
                        s
                      )
                Just (Dest y Nothing)
                  | p' == y && isDest' p' (fmap (+ 1) a) ->
                      ( Q.insert
                          r
                          ( Map.insert i (Dest p Nothing) $
                              Map.insert a (Dest y (Just p')) m
                          )
                          b,
                        Set.insert p' s
                      )
                _ -> (b, s)
            )
            (Q.empty, Set.empty)
          $ flooded (risk p')
  _ -> Q.empty
  where
    risk x = case x of
      A -> 1
      B -> 10
      C -> 100
      D -> 1000
    cond b = case m Map.!? b of
      Just (Dest c (Just c')) -> c /= c'
      _ -> False
    isDest' x j = case m Map.!? j of
      Nothing -> True
      Just (Dest y (Just z)) | x == y && x == z -> isDest' x (fmap (+ 1) j)
      _ -> False
    flooded j = flood j Set.empty [(r, i)] []
    flood :: Risk -> Set Index -> [(Int, Index)] -> [(Int, Index)] -> [(Int, Index)]
    flood risk = go
      where
        go visited start acc
          | null start = acc
          | otherwise = go visited' start' acc'
          where
            f k v =
              k `Set.notMember` v && case m Map.!? k of
                Just x | isNothing (getPod x) -> True
                _ -> False
            (start', visited') =
              foldl'
                ( \acc (r, (x, y)) ->
                    foldl'
                      ( \(s', v') (x', y') ->
                          let z = (x + x', y + y')
                           in if f z v'
                                then ((r + risk, z) : s', Set.insert z v')
                                else (s', v')
                      )
                      acc
                      adjacent
                )
                ([], visited)
                start
            acc' = start <> acc

adjacent :: [Index]
adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

draw :: Map Index Space -> String
draw =
  unlines
    . drawGraph
      ( \case Nothing -> ' '; Just x -> (case getPod x of Nothing -> '.'; Just y -> head (show y))
      )

day23 :: IO ()
day23 = do
  input <-
    drawMapWithKey
      ( \k a ->
          let n = case k of
                (x, 1) | x `elem` [3, 5, 7, 9] -> Just Space
                (3, y) | isJust m -> Just (Dest A)
                (5, y) | isJust m -> Just (Dest B)
                (7, y) | isJust m -> Just (Dest C)
                (9, y) | isJust m -> Just (Dest D)
                (x, 1) | x `elem` [1, 2, 4, 6, 8, 10, 11] -> Just Corridor
                _ -> Nothing
              m = case a of
                x | x `elem` "ABCD" -> Just (read @Pods [x])
                _ -> Nothing
           in ($ m) <$> n
      )
      . lines
      <$> readFile "input/input23.txt"
  input' <-
    drawMapWithKey
      ( \k a ->
          let n = case k of
                (x, 1) | x `elem` [3, 5, 7, 9] -> Just Space
                (3, y) | isJust m -> Just (Dest A)
                (5, y) | isJust m -> Just (Dest B)
                (7, y) | isJust m -> Just (Dest C)
                (9, y) | isJust m -> Just (Dest D)
                (x, 1) | x `elem` [1, 2, 4, 6, 8, 10, 11] -> Just Corridor
                _ -> Nothing
              m = case a of
                x | x `elem` "ABCD" -> Just (read @Pods [x])
                _ -> Nothing
           in ($ m) <$> n
      )
      . lines
      <$> readFile "input/input23'.txt"
  putStrLn $ ("day23a: " ++) $ show $ dijkstra Set.empty (Q.singleton 0 input)
  putStrLn $ ("day23b: " ++) $ show $ dijkstra Set.empty (Q.singleton 0 input')
