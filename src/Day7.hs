module Day7 where
import Data.List.Split (splitOn)
import Data.List (sort)

day7 :: IO ()
day7 = do
  input <- sort . map (read @Int) . splitOn "," <$> readFile "input/input7.txt"
  let mid = input !! ((length input `div` 2) - 1)
      span = [head input .. last input]
      sums = map (\x -> sum $ map (\y -> let d = abs (x - y) in (d * (d + 1)) `div` 2) input) span
  putStrLn $ ("day7a: " ++) $ show $ sum $ map (abs . subtract mid) input
  putStrLn $ ("day7b: " ++) $ show $ minimum sums
