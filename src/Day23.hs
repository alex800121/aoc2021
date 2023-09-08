{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day23 where

import Data.Bifunctor (bimap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
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

calcAddHeu :: Map Index Space -> Int
calcAddHeu =
  sum
    . map
      ( \case
          Dest x y | Just x /= y -> 10 ^ fromEnum x
          Dest x y | Just x == y -> negate (10 ^ fromEnum x)
          _ -> 0
      )
    . Map.elems

dijkstra :: Set (Map Index Space) -> MinPQueue Risk (Map Index Space) -> Maybe Risk
-- dijkstra acc q | traceShow (Q.size q) False = undefined
dijkstra acc q = case Q.minViewWithKey q of
  Nothing -> Nothing
  Just ((r, a), q') | a `Set.member` acc -> dijkstra acc q'
  Just ((r, a), q') ->
    -- if calcAddHeu a == 0
    if finished a
      then Just r
      else
        let q'' = Q.union q' $ Q.unions $ map (candidates (r, a)) $ Map.keys a
            acc' = Set.insert a acc
         in dijkstra acc' q''

aStar :: Set (Map Index Space) -> MinPQueue Heu (Risk, Map Index Space) -> Maybe Risk
-- aStar acc q | traceShow (Q.size q) False = undefined
aStar acc q = case Q.minViewWithKey q of
  Nothing -> Nothing
  Just ((h, (r, a)), q') | a `Set.member` acc -> aStar acc q'
  Just ((h, (r, a)), q') ->
    if finished a
      then Just r
      else
        let q'' = Q.union q' $ Q.unions $ map (candidates' (r, a)) $ Map.keys a
            acc' = Set.insert a acc
         in aStar acc' q''

-- in trace (draw a) $ aStar acc' q''

candidates' :: (Risk, Map Index Space) -> Index -> MinPQueue Heu (Risk, Map Index Space)
candidates' (r, m) i = case m Map.!? i of
  Just (Corridor (Just p)) ->
    Map.foldrWithKey
      (\k a b -> Q.insert (a + calcAddHeu k) (a, k) b)
      Q.empty
      $ Map.mapKeys
        ( \k ->
            let a = m Map.! k
                a' = a {getPod = Just p}
             in Map.insert i (Corridor Nothing) $ Map.insert k a' m
        )
      $ Map.filterWithKey (\k _ -> isDest p k)
      $ flooded p
  Just (Dest p (Just p'))
    | any cond [fmap (+ f) i | f <- [-3 .. 3]] ->
        Map.foldrWithKey
          (\k a b -> Q.insert (a + calcAddHeu k) (a, k) b)
          Q.empty
          $ Map.mapKeys
            ( \k ->
                let a = m Map.! k
                    a' = a {getPod = Just p'}
                 in Map.insert i (Dest p Nothing) $ Map.insert k a' m
            )
          $ Map.filterWithKey (\k _ -> isDest p k || isCorr k)
          $ flooded p'
  _ -> Q.empty
  where
    cond b = case m Map.!? b of
      Just (Dest c (Just c')) -> c /= c'
      _ -> False
    flooded j = flood j (Map.singleton i r) Map.empty
    isDest x j = case m Map.!? j of
      Just (Dest y Nothing) | x == y -> isDest' x (fmap (+ 1) j)
      _ -> False
    isDest' x j = case m Map.!? j of
      Nothing -> True
      Just (Dest y (Just z)) | x == y && x == z -> isDest' x (fmap (+ 1) j)
      _ -> False
    isCorr j = case m Map.!? j of
      Just (Corridor Nothing) -> True
      _ -> False
    flood :: Pods -> Map Index Int -> Map Index Int -> Map Index Int
    flood y start acc
      | Map.null start = acc
      | otherwise = flood y start' acc'
      where
        risk = case y of
          A -> 1
          B -> 10
          C -> 100
          D -> 1000
        start' =
          Map.filterWithKey
            ( \k _ ->
                k `Map.notMember` acc && case m Map.!? k of
                  Just x | isNothing (getPod x) -> True
                  _ -> False
            )
            $ Set.fold Map.union Map.empty
            $ Set.map (\(x, y) -> Map.map (+ risk) $ Map.mapKeys (bimap (+ x) (+ y)) start) adjacent
        acc' = Map.union start acc

candidates :: (Risk, Map Index Space) -> Index -> MinPQueue Risk (Map Index Space)
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
    -- \| condUp (fmap (subtract 1) i) && condDown i ->
    | any cond [fmap (+ f) i | f <- [-3 .. 3]] ->
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
    isDest x j = case m Map.!? j of
      Just (Dest y Nothing) | x == y -> isDest' x (fmap (+ 1) j)
      _ -> False
    isDest' x j = case m Map.!? j of
      Nothing -> True
      Just (Dest y (Just z)) | x == y && x == z -> isDest' x (fmap (+ 1) j)
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
            ( \k ->
                k `Set.notMember` visited && case m Map.!? k of
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
      -- <$> readFile "input/test23.txt"
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
      -- <$> readFile "input/test23'.txt"
      <$> readFile "input/input23'.txt"
  -- mapM_ (\x -> print (fst x) >> putStrLn (draw (snd x))) $ concatMap (Q.toList . candidates (0, input)) $ Map.keys input
  -- putStrLn $ draw input
  -- putStrLn $ ("day23a: " ++) $ show $ dijkstra Set.empty (Q.singleton 0 input)
  -- putStrLn $ ("day23b: " ++) $ show $ dijkstra Set.empty (Q.singleton 0 input')
  putStrLn $ ("day23b: " ++) $ show $ aStar Set.empty (Q.singleton (calcAddHeu input') (0, input'))
  -- putStrLn $ ("day23a: " ++) $ show $ aStar Set.empty (Q.singleton (calcAddHeu input) (0, input))
