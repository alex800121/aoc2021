module Day21 where
import Data.List.Split (divvy)
import Debug.Trace

player1 = 4
player2 = 8
-- player1 = 6
-- player2 = 2


diceList = [1 .. 100]

data GameState = G
  { turn1 :: Bool,
    player1Place :: Int,
    player1Score :: Int,
    player2Place :: Int,
    player2Score :: Int
  } deriving (Show, Eq, Ord)

initGameState = G True player1 0 player2 0

playGame :: [Int] -> Int -> GameState -> Int
playGame dl time g
  | trace (show (time, g)) False = undefined
  | player1Score g >= 1000 = player2Score g * time
  | player2Score g >= 1000 = player1Score g * time
  | turn1 g = let n = ((player1Place g + head dl - 1) `mod` 10) + 1 in playGame (tail dl) (time + 1) $ g { player1Place = n, turn1 = False, player1Score = player1Score g + n }
  | otherwise = let n = ((player2Place g + head dl - 1) `mod` 10) + 1 in playGame (tail dl) (time + 1) $ g { player2Place = n, turn1 = True, player2Score = player2Score g + n }

day21 :: IO ()
day21 = do
  print $ playGame (map sum (divvy 3 3 (cycle diceList))) 0 initGameState
