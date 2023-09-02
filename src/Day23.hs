module Day23 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Data.Set (Set)
import qualified Data.Set as Set

type Index = (Int, Int)

type Risk = Int

data Pods = A | B | C | D deriving (Show, Eq, Ord)

data Space
  = Corridor {getPod :: Maybe Pods}
  | Dest {dest :: Pods, getPod :: Maybe Pods}
  | Space {getPod :: Maybe Pods}
  deriving (Show, Eq, Ord)

candidates :: Map Index Space -> Index -> [Map Index Space]
candidates m i = case m Map.!? i of
  Just (Corridor (Just p)) -> undefined
  Just (Dest p (Just p'))
    | p /= p' || any cond [fmap (+ 1) i, fmap (subtract 1) i] ->
        undefined
  _ -> []
  where
    cond b = case m Map.!? b of
      Just (Dest c (Just c')) -> c /= c'
      _ -> False

adjacent :: [Index]
adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

day23 :: IO ()
day23 = return ()
