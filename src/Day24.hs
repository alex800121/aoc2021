module Day24 where

import Paths_AOC2021
import Data.Char (intToDigit)
import Paths_AOC2021
import Data.List ((\\))
import Paths_AOC2021
import Data.List.Split (chunksOf)
import Paths_AOC2021
import Data.Maybe (fromJust, mapMaybe)
import Paths_AOC2021
import MyLib ((!?))
import Paths_AOC2021
import Text.Read (readMaybe)

data Ins = Ins
  { z :: Int,
    x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)

readIns :: [String] -> Maybe Ins
readIns input = do
  z <- input !? 4 >>= (!? 2) . words >>= readMaybe
  x <- input !? 5 >>= (!? 2) . words >>= readMaybe
  y <- input !? 15 >>= (!? 2) . words >>= readMaybe
  Just $ Ins z x y

solve :: [Int] -> [Ins] -> Maybe [Int]
solve = go 0 []
  where
    update i x l
      | i < 0 || i >= length l = l
      | otherwise = take i l ++ x : drop (i + 1) l
    go _ [] inp [] = Just inp
    go _ _ _ [] = Nothing
    go i stack inp (Ins 1 _ y : ins) = go (i + 1) ((i, y) : stack) inp ins
    go i ((j, y) : stack) inp (Ins 26 x _ : ins)
      | inp_i > 9 = go (i + 1) stack (update i 9 $ update j j0 inp) ins
      | inp_i < 1 = go (i + 1) stack (update i 1 $ update j j1 inp) ins
      | otherwise = go (i + 1) stack (update i inp_i inp) ins
      where
        inp_i = inp !! j + y + x
        j0 = inp !! j - (inp_i - 9)
        j1 = inp !! j + (1 - inp_i)

-- stack needs to be popped
-- => w == stack_pop + yadd + xcheck

day24 :: IO ()
day24 = do
  input <- mapMaybe readIns . chunksOf (252 `div` 14) . lines <$> readFile "input/input24.txt"
  putStrLn
    . ("day24a: " ++)
    . map intToDigit
    . fromJust
    $ solve (replicate 14 9) input
  putStrLn
    . ("day24b: " ++)
    . map intToDigit
    . fromJust
    $ solve (replicate 14 1) input
