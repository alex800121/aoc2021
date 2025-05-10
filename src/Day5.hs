module Day5 where

import Control.Monad.ST.Strict (ST, runST)
import Data.Array.IArray qualified as A
import Data.Array.Unboxed (Array)
import Data.List.Split qualified as Split
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as M
import Data.Word (Word8)
import Paths_AOC2021

type Index = (Int, Int)

type Ends = (Index, Index)

straight ((sx, sy), (ex, ey)) = sy == ey || sx == ex

drawLine :: STVector s Int -> Ends -> ST s ()
drawLine v ((sx, sy), (ex, ey)) = mapM_ (f v) l
  where
    dx = signum (ex - sx)
    dy = signum (ey - sy)
    lx = [sx, sx + dx .. ex]
    ly = [sy, sy + dy .. ey]
    l = zipWith (\x y -> x * 1000 + y) lx ly
    f v = M.modify v succ

run ls = runST $ do
  v <- M.replicate (1000 * 1000) 0
  mapM_ (drawLine v) ls
  M.foldl' (\acc x -> if x > 1 then succ acc else acc) 0 v

inputParser =
  map
    ( (\(x : y : _) -> (x, y))
        . map ((\(x : y : _) -> (x, y)) . map (read @Int) . Split.splitOn ",")
        . Split.splitOn " -> "
    )
    . lines

day5 :: IO ()
day5 = do
  input <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  putStrLn
    . ("day5a: " ++)
    . show
    . run
    $ filter straight input
  putStrLn
    . ("day5b: " ++)
    . show
    $ run input
