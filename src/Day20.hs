module Day20 where

import Control.Monad
import Control.Monad.ST.Strict (ST, runST)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.IArray qualified as A
import Data.Array.MArray qualified as MA
import Data.Array.ST (STUArray)
import Data.Array.ST qualified as MA
import Data.Array.Unboxed (Array, UArray)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.List.Split (divvy, splitOn)
import Data.Maybe (fromMaybe)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable (STVector)
import Data.Vector.Storable.Mutable qualified as M
import Data.Vector.Strict.Mutable qualified as SM
import Data.WideWord
import Debug.Trace
import MyLib (drawArray)
import Paths_AOC2021

type Trench = (Int, [Word256])

type Rule = Vector Bool

inputParser s = (V.fromList (map (== '#') $ filter (/= '\n') x), ((width, height), a))
  where
    [x, y] = splitOn "\n\n" s
    ys = lines y
    width = length (head ys)
    height = length ys
    a = map (f (0 :: Word256) (width - 1)) ys
    f acc _ [] = acc
    f acc i ('#' : cs) = f (acc `setBit` i) (pred i) cs
    f acc i (_ : cs) = f acc (pred i) cs

step' :: (Int, Int) -> Rule -> SM.STVector s (STVector s Word256) -> STVector s Word256 -> Int -> ST s ()
step' (initWidth, initHeight) rule ref v gen = do
  mapM_ (SM.read ref >=> \x -> mapM_ (\j -> M.write x j (if odd gen then 7 else 0)) [0 .. width + 1]) [0, 1, height, height + 1]
  mapM_
    ( \i -> do
        r0 <- SM.read ref i
        r1 <- SM.read ref (i + 1)
        r2 <- SM.read ref (i + 2)
        x <- M.read v i
        let fixX
              | odd gen = ((3 .|. (x `shiftL` 2)) `setBit` width) `setBit` (width + 1)
              | otherwise = x `shiftL` 2
        foldM_
          ( \acc j -> do
              let c = acc .&. 7
              M.write r2 j c
              a <- M.read r0 j
              b <- M.read r1 j
              let d = (a `shiftL` 6) .|. (b `shiftL` 3) .|. c
                  t = rule V.! fromIntegral d
              M.modify v (if t then (`setBit` j) else (`clearBit` j)) i
              pure (acc `shiftR` 1)
          )
          fixX
          [0 .. width - 1]
    )
    [0 .. height - 3]
  mapM_
    ( \i -> do
        r0 <- SM.read ref i
        r1 <- SM.read ref (i + 1)
        r2 <- SM.read ref (i + 2)
        mapM_
          ( \j -> do
              a <- M.read r0 j
              b <- M.read r1 j
              c <- M.read r2 j
              let d = (a `shiftL` 6) .|. (b `shiftL` 3) .|. c
                  t = rule V.! fromIntegral d
              M.modify v (if t then (`setBit` j) else (`clearBit` j)) i
          )
          [0 .. width - 1]
    )
    [height - 2 .. height - 1]
  where
    width = initWidth + 2 * (gen + 1)
    height = initHeight + 2 * (gen + 1)

run :: (Int, Int) -> Rule -> Int -> Vector Word256 -> Int
run d rule i v = runST $ do
  v0 <- V.thaw (V.concat [v, V.replicate (i * 2) 0])
  ref <- SM.replicateM (M.length v0 + 4) (M.replicate 256 0)
  mapM_ (step' d rule ref v0) [0 .. i - 1]
  M.foldl' (\acc x -> acc + popCount x) 0 v0

step :: Bool -> Int -> Rule -> Trench -> Trench
step isTest initWidth rule (gen, v0) = (nextGen, vF)
  where
    nextGen = succ gen
    nextWidth = initWidth + nextGen * 2
    ones8 = if isTest then zeros8 else [replicate nextWidth 7, replicate nextWidth 7]
    zeros8 = [replicate nextWidth 0, replicate nextWidth 0]
    oneBits = (3 `setBit` nextWidth) `setBit` (nextWidth + 1)
    vRef
      | not isTest && gen `testBit` 0 = concat [ones8, map (calc8Bits nextWidth [] . (.|. oneBits) . (`shiftL` 2)) v0, ones8]
      | otherwise = concat [zeros8, map (calc8Bits nextWidth [] . (`shiftL` 2)) v0, zeros8]
    f [x0, x1, x2] = g 0 (nextWidth - 1) (zipWith3 (\a b c -> (a `shiftL` 6) .|. (b `shiftL` 3) .|. c) x0 x1 x2)
    vF = parMap rpar f $ divvy 3 1 vRef
    g acc i (x : xs) = g acc' (pred i) xs
      where
        acc' = if rule V.! fromIntegral x then acc `setBit` i else acc
    g acc _ _ = acc

printer initWidth (g, v) = unlines $ parMap rpar (go "" w) v
  where
    w = initWidth + 2 * g
    go acc i x
      | i <= 0 = acc
      | otherwise = go (c : acc) (pred i) (x `shiftR` 1)
      where
        c = if x `testBit` 0 then '#' else '.'

solve initWidth (g, v) = sum (parMap rpar (go 0 w) v)
  where
    w = initWidth + 2 * g
    go acc i x
      | i <= 0 = acc
      | otherwise = go acc' (pred i) (x `shiftR` 1)
      where
        acc' = if x `testBit` 0 then acc + 1 else acc

calc8Bits w acc x
  | w <= 0 = acc
  | otherwise = calc8Bits (pred w) ((x .&. 7) : acc) (x `shiftR` 1)

day20 :: IO ()
day20 = do
  (rule, (d@(initWidth, initHeight), pic)) <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  putStrLn
    . ("day20a: " ++)
    . show
    $ run d rule 2 (V.fromList pic)
  putStrLn
    . ("day20b: " ++)
    . show
    $ run d rule 50 (V.fromList pic)

-- 5846
-- 21149
