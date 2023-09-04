{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day23 where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, isJust)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (drawGraph, drawMapWithKey)
import Debug.Trace

type Index = (Int, Int)

type Risk = Int

data Pods = A | B | C | D deriving (Show, Eq, Ord, Read)

data Space
  = Corridor {getPod :: Maybe Pods}
  | Dest {dest :: Pods, getPod :: Maybe Pods}
  | Space {getPod :: Maybe Pods}
  deriving (Show, Eq, Ord)

finished :: Map Index Space -> Bool
finished = all (\case
  Dest x y -> Just x == y
  x -> isNothing (getPod x)) . Map.elems

dijkstra :: Set (Map Index Space) -> MinPQueue Risk (Map Index Space) -> Maybe Risk
dijkstra acc q = case Q.minViewWithKey q of
  Nothing -> Nothing
  Just ((r, a), q') | a `Set.member` acc -> dijkstra acc q'
  Just ((r, a), q') | a `Set.notMember` acc -> if finished a then Just r else let
    q'' = Q.union q' $ Q.unions $ map (candidates (r, a)) $ Map.keys a
    acc' = Set.insert a acc
    in dijkstra acc' q''
    -- in trace (draw a) $ dijkstra acc' q''

candidates :: (Risk, Map Index Space) -> Index -> MinPQueue Risk (Map Index Space)
-- candidates m i | trace (show i ++ '\n' : draw m) False = undefined
candidates (r, m) i = case m Map.!? i of
  Just (Corridor (Just p)) ->
    Q.map
      ( \k ->
          let a = m Map.! k
              a' = a {getPod = Just p}
           in Map.insert i (Corridor Nothing) $ Map.insert k a' m
      )
      $ Q.filter (isDest p)
      $ flooded p
  Just (Dest p (Just p'))
    | p /= p' || any cond [fmap f i | f <- [subtract 1, (+ 1)]] ->
        Q.map
          ( \k ->
              let a = m Map.! k
                  a' = a {getPod = Just p'}
               in Map.insert i (Dest p Nothing) $ Map.insert k a' m
          )
          $ Q.filter (\k -> isDest p k || isCorr k)
          $ flooded p'
  _ -> Q.empty
  where
    cond b = case m Map.!? b of
      Just (Dest c (Just c')) -> c /= c'
      _ -> False
    flooded j = flood j Set.empty (Q.singleton r i) Q.empty
    isDest x j = case (m Map.!? j, m Map.!? fmap (+ 1) j) of
      (Just (Dest y Nothing), Just (Dest z (Just w))) | x == y && x == z && x == w -> True
      (Just (Dest y Nothing), Nothing) | x == y -> True
      _ -> False
    isCorr j = case m Map.!? j of
      Just (Corridor Nothing) -> True
      _ -> False
    flood :: Pods -> Set Index -> MinPQueue Int Index -> MinPQueue Int Index -> MinPQueue Int Index
    flood y visited start acc
      | Q.null start = acc
      | otherwise = flood y visited' start' acc'
      where
        risk = case y of
          A -> 1
          B -> 10
          C -> 100
          D -> 1000
        start' =
          Q.filter
            ( \k -> k `Set.notMember` visited && case m Map.!? k of
                Just x | isNothing (getPod x) -> True
                _ -> False
            )
            $ Set.fold Q.union Q.empty
            $ Set.map (\(x, y) -> Q.mapKeys (+ risk) $ Q.map (bimap (+ x) (+ y)) start) adjacent
        acc' = Q.union start acc
        visited' = foldr Set.insert visited $ Q.elems start

adjacent :: Set Index
adjacent = Set.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

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
      <$> readFile "input/test23.txt"
      -- <$> readFile "input/input23.txt"
      -- <$> readFile "input/test23'.txt"
      -- <$> readFile "input/input23'.txt"
  -- mapM_ (\x -> print (fst x) >> putStrLn (draw (snd x))) $ concatMap (Q.toList . candidates (0, input)) $ Map.keys input
  -- putStrLn $ draw input
  print $ dijkstra Set.empty (Q.singleton 0 input)
