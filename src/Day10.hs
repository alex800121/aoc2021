module Day10 where

import Paths_AOC2021
import Data.Char (isSpace)
import Paths_AOC2021
import Data.Either (fromLeft, fromRight, isRight)
import Paths_AOC2021
import Data.List (foldl', sort)
import Paths_AOC2021
import GHC.List (uncons)

findCorrupted :: String -> String -> Either Int Int
findCorrupted ys [] =
  Right
    ( foldl'
        ( \acc x ->
            (acc * 5) + case x of
              '(' -> 1
              '[' -> 2
              '{' -> 3
              '<' -> 4
        )
        0
        ys
    )
findCorrupted ys (x : xs) = case (x, uncons ys) of
  ('}', Just ('{', ys)) -> findCorrupted ys xs
  (']', Just ('[', ys)) -> findCorrupted ys xs
  (')', Just ('(', ys)) -> findCorrupted ys xs
  ('>', Just ('<', ys)) -> findCorrupted ys xs
  ('}', _) -> Left 1197
  (']', _) -> Left 57
  (')', _) -> Left 3
  ('>', _) -> Left 25137
  (x, _) | x `elem` "{[(<" -> findCorrupted (x : ys) xs

day10 :: IO ()
day10 = do
  input <- lines <$> readFile "input/input10.txt"
  putStrLn
    . ("day10a: " ++)
    . show
    . sum
    $ map (fromLeft 0 . findCorrupted "") input
  putStrLn
    . ("day10b: " ++)
    . show
    . (\x -> x !! (length x `div` 2))
    . sort
    . map (fromRight 0)
    . filter isRight
    $ map (findCorrupted "") input
