module Day2 where

import Data.List (foldl')

data Instruction = Forward Int | Up Int | Down Int deriving (Eq, Show)
type Submarine = (Int, Int)
type SubmarineAim = (Int, Int, Int)

parseInput :: String -> Instruction
parseInput s = case x of
  "up" -> Up i
  "down" -> Down i
  _ -> Forward i
  where
    (x : y : _) = words s
    i = read y

moveSubmarine :: Submarine -> Instruction -> Submarine
moveSubmarine (x, y) i = case i of
  Forward n -> (x + n, y)
  Up n -> (x, y - n)
  Down n -> (x, y + n)

moveSubmarineAim :: SubmarineAim -> Instruction -> SubmarineAim
moveSubmarineAim (x, y, aim) i = case i of
  Forward n -> (x + n, y + (n * aim), aim)
  Up n -> (x, y, aim - n)
  Down n -> (x, y, aim + n)

day2 :: IO ()
day2 = do
  input <- map parseInput . lines <$> readFile "input/input2.txt"
  putStrLn $ ("day2a: " ++) $ show $ uncurry (*) $ foldl' moveSubmarine (0, 0) input
  putStrLn $ ("day2b: " ++) $ show $ (\(x, y, _) -> x * y) $ foldl' moveSubmarineAim (0, 0, 0) input
