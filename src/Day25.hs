{-# LANGUAGE LambdaCase #-}

module Day25 where

import Paths_AOC2021
import Data.Bifunctor (bimap)
import Paths_AOC2021
import Data.Either (isLeft)
import Paths_AOC2021
import qualified Data.Either as Set
import Paths_AOC2021
import Data.Map (fromSet, keysSet, partition, union)
import Paths_AOC2021
import Data.Set (Set)
import Paths_AOC2021
import qualified Data.Set as Set
import Paths_AOC2021
import MyLib (drawGraph, drawMap, firstRepeat)

type Index = (Int, Int)

data SC = SC {_height :: Int, _width :: Int, _east :: Set Index, _south :: Set Index}
  deriving (Show, Eq, Ord)

run :: (SC -> Int) -> ((Int -> Int -> Int) -> Int -> Index -> Index) -> (SC -> Set Index) -> (SC -> Set Index) -> (Set Index -> SC -> SC) -> SC -> SC
run f1 f2 f3 f4 f5 sc = f5 e' sc
  where
    w = f1 sc
    f x = let a = f2 (+) w x in if a `Set.member` f4 sc || a `Set.member` f3 sc then x else a
    e' = Set.map f $ f3 sc
    -- f x = let a = f2 (+) w x in if a `Set.member` f4 sc then Left x else Right a
    -- (l, r) =
    --   Set.fold
    --     ( \x (accx, accy) -> case f x of
    --         Left x -> (Set.insert x accx, accy)
    --         Right x -> (accx, Set.insert x accy)
    --     )
    --     (Set.empty, Set.empty)
    --     $ f3 sc
    -- g (x, y)
    --   | Set.null x = y
    --   | otherwise = g (x', yr `Set.union` x)
    --   where
    --     (yl, yr) = Set.partition (`Set.member` x) y
    --     x' = Set.map (f2 (-) w) yl
    -- e' = g (l, r)

runEast = run _width (\f w (x, y) -> (f x 1 `mod` w, y)) _east _south (\x sc -> sc {_east = x})

runSouth = run _height (\f h (x, y) -> (x, f y 1 `mod` h)) _south _east (\x sc -> sc {_south = x})

printSC :: SC -> String
printSC sc = unlines $ drawGraph (\case Nothing -> '.'; Just True -> '>'; Just False -> 'v') $ union (fromSet (const True) (_east sc)) (fromSet (const False) (_south sc))

day25 :: IO ()
day25 = do
  -- input <- lines <$> readFile "input/test25.txt"
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  let h = length input
      w = length (head input)
      (e, s) = bimap keysSet keysSet $ partition id $ drawMap (\case '>' -> Just True; 'v' -> Just False; _ -> Nothing) input
      sc = SC h w e s
  putStrLn $ ("day25a: " ++) $ show $ fmap fst $ firstRepeat $ iterate (runSouth . runEast) sc
  putStrLn "Merry Christmas!!!"
