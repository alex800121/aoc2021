module Day20 where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array, UArray)
import Data.Bits (Bits (..), FiniteBits (..))
import Data.List.Split (divvy, splitOn)
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.WideWord
import MyLib (drawArray)
import Paths_AOC2021

type Trench = (Int, [Word256])

type Rule = Vector Bool

inputParser s = (V.fromList (map (== '#') $ filter (/= '\n') x), (width, a))
  where
    [x, y] = splitOn "\n\n" s
    ys = lines y
    width = length (head ys)
    a = map (f (0 :: Word256) (width - 1)) ys
    f acc _ [] = acc
    f acc i ('#' : cs) = f (acc `setBit` i) (pred i) cs
    f acc i (_ : cs) = f acc (pred i) cs

step :: Int -> Rule -> Trench -> Trench
step initWidth rule (gen, v0) = (nextGen, vF)
  where
    nextGen = succ gen
    nextWidth = initWidth + nextGen * 2
    ones8 = [replicate nextWidth 7, replicate nextWidth 7]
    zeros8 = [replicate nextWidth 0, replicate nextWidth 0]
    oneBits = (3 `setBit` nextWidth) `setBit` (nextWidth + 1)
    vRef
      | gen `testBit` 0 = concat [ones8, map (calc8Bits nextWidth [] . (.|. oneBits) . (`shiftL` 2)) v0, ones8]
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
  (rule, (initWidth, pic)) <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  putStrLn
    . ("day20b: " ++)
    . show
    . solve initWidth
    $ (!! 2)
    $ iterate (step initWidth rule) (0, pic)
  putStrLn
    . ("day20b: " ++)
    . show
    . solve initWidth
    $ (!! 50)
    $ iterate (step initWidth rule) (0, pic)
