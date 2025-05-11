module Day8 where

import Data.Bifunctor (Bifunctor (first))
import Data.Bits (Bits (..))
import Data.Char (chr, ord)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.List (elemIndex, find, findIndex, foldl', permutations, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import MyLib (mapFirst)
import Paths_AOC2021 (getDataDir)

original :: [([Int], Int)]
original =
  zip
    [ [0, 1, 2, 4, 5, 6],
      [2, 5],
      [0, 2, 3, 4, 6],
      [0, 2, 3, 5, 6],
      [1, 2, 3, 5],
      [0, 1, 3, 5, 6],
      [0, 1, 3, 4, 5, 6],
      [0, 2, 5],
      [0, 1, 2, 3, 4, 5, 6],
      [0, 1, 2, 3, 5, 6]
    ]
    [0 .. 9]

originalString = "abcdefg"

encodeString :: String -> Int
encodeString = foldl' (\acc x -> acc `setBit` (ord x - ord 'A')) 0

fromOriginal :: String -> IntMap Int
-- fromOriginal st = traceShow ans ans
fromOriginal st = ans
  where
    ans = IM.fromList $ map (first (encodeString . map (st !!))) original

decode :: IntMap Int -> [String] -> [Int]
decode stringID = map ((stringID IM.!) . encodeString)

day8 :: IO ()
day8 = do
  input <-
    map
      ( (\(x : y : _) -> (sort x, y)) . map (map sort . words) . splitOn " | "
      )
      . lines
      <$> (getDataDir >>= readFile . (++ "/input/input8.txt"))
  let strings = map fromOriginal (permutations originalString)
      stringIDs = map (first (\x -> fromJust (find (\y -> all ((`IM.member` y) . encodeString) x) strings))) input
      decoded = map (uncurry decode) stringIDs
  putStrLn
    . ("day8a: " ++)
    . show
    . length
    . filter (`elem` [1, 4, 7, 8])
    $ concat decoded
  putStrLn
    . ("day8b: " ++)
    . show
    . sum
    $ map (foldl' (\acc x -> (acc * 10) + x) 0) decoded
