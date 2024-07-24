module Day8 where

import Paths_AOC2021
import Data.Char (chr, ord)
import Paths_AOC2021
import Data.List (elemIndex, find, findIndex, foldl', permutations, sort)
import Paths_AOC2021
import Data.List.Split (splitOn)
import Paths_AOC2021
import Data.Maybe (catMaybes, fromJust)
import Paths_AOC2021
import MyLib (mapFirst)
import Paths_AOC2021
import qualified Data.Map as Map
import Paths_AOC2021
import Data.Map (Map)

original :: Map [Int] Int
original = 
  Map.fromList $ zip
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
    [0..9]

originalString = "abcdefg"

fromOriginal :: String -> Map String Int
fromOriginal st = Map.mapKeys (sort . map (st !!)) original

decode :: Map String Int -> [String] -> [Int]
decode stringID numbers = map (stringID Map.!) numbers

day8 :: IO ()
day8 = do
  input <-
    map
      ( (\(x : y : _) -> (sort x, y)) . map (map sort . words) . splitOn " | "
      )
      . lines
      <$> readFile "input/input8.txt"
  let strings = map fromOriginal (permutations originalString)
      stringIDs = map (mapFirst (\x -> fromJust (find (\y -> all (`Map.member` y) x) strings))) input
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
