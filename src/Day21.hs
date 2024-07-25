module Day21 where

import Data.Bifunctor (bimap)
import Data.List.Split (divvy)
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as MS
import Debug.Trace
import Paths_AOC2021

-- player1 = 4
-- player2 = 8
player1 = 6

player2 = 2

diceList = [1 .. 100]

-- diracDice = [1 .. 3]
diracDice = MS.toOccurList $ MS.fromList $ [a + b + c | let x = [1 .. 3], a <- x, b <- x, c <- x]

data GameState = G
  { turn1 :: Bool,
    player1Place :: Int,
    player1Score :: Int,
    player2Place :: Int,
    player2Score :: Int
  }
  deriving (Show, Eq, Ord)

initGameState = G True player1 0 player2 0

playGame :: [Int] -> Int -> Int -> GameState -> Int
playGame dl goal time g
  -- \| trace (show (time, g)) False = undefined
  | player1Score g >= goal = player2Score g * time
  | player2Score g >= goal = player1Score g * time
  | turn1 g = let n = ((player1Place g + head dl - 1) `mod` 10) + 1 in playGame (tail dl) goal (time + 1) $ g {player1Place = n, turn1 = False, player1Score = player1Score g + n}
  | otherwise = let n = ((player2Place g + head dl - 1) `mod` 10) + 1 in playGame (tail dl) goal (time + 1) $ g {player2Place = n, turn1 = True, player2Score = player2Score g + n}

rollDice :: GameState -> Int -> Either Bool GameState
rollDice g d
  | turn1 g =
      let n = ((player1Place g + d - 1) `mod` 10) + 1
          g' = g {player1Place = n, turn1 = False, player1Score = player1Score g + n}
       in if player1Score g' >= 21 then Left True else Right g'
  | otherwise =
      let n = ((player2Place g + d - 1) `mod` 10) + 1
          g' = g {player2Place = n, turn1 = True, player2Score = player2Score g + n}
       in if player2Score g' >= 21 then Left False else Right g'

calcWin :: MultiSet GameState -> (Integer, Integer) -> (Integer, Integer)
calcWin gs acc
  | MS.null gs = acc
  | otherwise = calcWin gs' (bimap (+ p1Win) (+ p2Win) acc)
  where
    (x, gs') = bimap MS.unions MS.unions $ unzip $ concatMap (\(d, n) -> replicate n $ MS.mapEither (`rollDice` d) gs) diracDice
    p1Win = fromIntegral $ MS.occur True x
    p2Win = fromIntegral $ MS.occur False x

day21 :: IO ()
day21 = do
  putStrLn $ ("day21a: " ++) $ show $ 3 * playGame (map sum (divvy 3 3 (cycle diceList))) 1000 0 initGameState
  putStrLn $ ("day21b: " ++) $ show $ uncurry max $ calcWin (MS.singleton initGameState) (0, 0)

-- print diracDice
