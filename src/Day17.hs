module Day17 where

type Range = (Int, Int)

type Index = (Int, Int)

type Velocity = (Int, Int)

type Target = (Range, Range)

targetArea = ((288, 330), (-96, -50))

-- targetArea = ((20, 30), (-10, -5))

withinTarget :: Target -> Index -> Velocity -> Bool
withinTarget target@((minX, maxX), (minY, maxY)) (x, y) (vX, vY)
  | x > maxX || y < minY = False
  | x >= minX && y <= maxY = True
  | otherwise = withinTarget target (x + vX, y + vY) (max 0 (vX - 1), vY - 1)

yRange = [(fst (snd targetArea)) .. (negate (fst (snd targetArea)) - 1)]

xRange = dropWhile (\x -> (x * (x + 1)) `div` 2 < fst (fst targetArea)) [1 .. (snd (fst targetArea))]

day17 :: IO ()
day17 = do
  putStrLn $ ("day17a: " ++) $ show $ (95 * 96) `div` 2
  putStrLn $ ("day17b: " ++) $ show $ length $ filter (withinTarget targetArea (0, 0)) [(x, y) | x <- xRange, y <- yRange]
