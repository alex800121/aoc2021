module Day23 where

import Data.Char (chr, isAlpha, ord)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List (find, findIndex, foldl')
import Data.PQueue.Prio.Min (MinPQueue (..))
import Data.PQueue.Prio.Min qualified as Q
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Debug.Trace (traceShow)
import Paths_AOC2021 (getDataDir)

type GameState = (Int, Vector Int)

{-
00 01 02 03 04 05 06 07 08 09 10
      11    13    15    17
      12    14    16    18
-}

hallwayIndex = [0, 1, 3, 5, 7, 9, 10]

hToRChoices :: GameState -> [(Int, GameState)]
hToRChoices (cost, v) =
  [ (hue, (cost', v'))
    | h <- hallwayIndex,
      let pod = v V.! h,
      pod /= 0,
      let room = pod - 1,
      availRoom v room,
      let p = path h (room * 2 + 2),
      pathClear v h p,
      let vacant = calcVacant v room,
      let r = 11 + room * depth v + vacant - 1,
      let cost' = cost + (vacant + length p - 1) * (10 ^ (pod - 1)),
      let v' = v V.// [(h, 0), (r, pod)],
      let hue = cost' + calcHue v'
  ]

showVec :: Vector Int -> String
showVec v = s0 ++ '\n' : go 0 0
  where
    d = depth v
    f 0 = '.'
    f x = chr (x - 1 + ord 'A')
    s0 = take 11 $ map f $ V.toList v
    go r de
      | de >= d = ""
      | r >= 4 = '\n' : go 0 (succ de)
      | r == 0 = "  " ++ f (v V.! i) : go (r + 1) de
      | otherwise = ' ' : f (v V.! i) : go (r + 1) de
      where
        i = 11 + r * d + de

rToRChoices :: GameState -> [(Int, GameState)]
rToRChoices (cost, v) =
  [ (targetHue, (targetCost, targetV))
    | room <- [0 .. 3],
      not (availRoom v room),
      let vacant = calcVacant v room,
      let roomHall = room * 2 + 2,
      let podLoc = 11 + room * d + vacant,
      let pod = v V.! podLoc,
      let targetRoom = pod - 1,
      room /= targetRoom,
      availRoom v targetRoom,
      let targetHall = targetRoom * 2 + 2,
      let targetPath = path targetHall roomHall,
      pathClear v targetHall targetPath,
      let targetVacant = calcVacant v targetRoom,
      let targetLoc = 11 + targetRoom * d + targetVacant - 1,
      let targetV = v V.// [(targetLoc, pod), (podLoc, 0)],
      let targetCost = cost + (vacant + length targetPath + targetVacant) * (10 ^ (pod - 1)),
      let targetHue = targetCost + calcHue targetV
  ]
  where
    d = depth v

rToHChoices :: GameState -> [(Int, GameState)]
rToHChoices (cost, v) =
  [ (hue, (cost', v'))
    | room <- [0 .. 3],
      not (availRoom v room),
      h <- hallwayIndex,
      v V.! h == 0,
      let vacant = calcVacant v room,
      vacant < 4,
      let r = 11 + room * depth v + vacant,
      let pod = v V.! r,
      let p = path h (room * 2 + 2),
      pathClear v h p,
      let cost' = cost + (vacant + length p) * (10 ^ (pod - 1)),
      let v' = v V.// [(h, pod), (r, 0)],
      not (mutualBlock v'),
      not (leftBlock v'),
      not (rightBlock v'),
      let hue = cost' + calcHue v'
  ]

depth v = (V.length v - 11) `div` 4

path h r = [min h r .. max h r]

pathClear v h = all f
  where
    f x = x == h || v V.! x == 0

calcVacant v room = length $ takeWhile (\x -> v V.! (11 + room * d + x) == 0) [0 .. d - 1]
  where
    d = depth v

availRoom v room = all f [0 .. d - 1]
  where
    d = depth v
    pod = room + 1
    f x = y == 0 || y == pod
      where
        y = v V.! (11 + room * d + x)

inputParser :: String -> Vector Int
inputParser s = V.replicate l 0 V.// go 0 0 b
  where
    l = length s
    depth = (l - 11) `div` 4
    (a, b) = splitAt 11 s
    f '.' = 0
    f x = ord x - ord 'A' + 1
    go _ _ [] = zip [0 ..] (map f a)
    go r d (x : xs)
      | r >= 4 = go 0 (succ d) (x : xs)
      | otherwise = (11 + depth * r + d, f x) : go (succ r) d xs

complete v =
  and
    [ pod == room + 1
      | room <- [0 .. 3],
        d <- [0 .. depth v - 1],
        let pod = v V.! (11 + room * depth v + d)
    ]

rightBlock v = pod7 == 4 && vacantHall < outRoomD
  where
    d = depth v
    pod7 = v V.! 7
    roomD = [10 + d, 9 + d .. 11]
    outRoomD = d - length (takeWhile (\x -> v V.! x == 4) roomD)
    vacantHall = length (filter (\x -> v V.! x == 0) [9, 10])

leftBlock v = pod3 == 1 && vacantHall < outRoomA
  where
    d = depth v
    pod3 = v V.! 3
    roomA = [10 + d, 9 + d .. 11]
    outRoomA = d - length (takeWhile (\x -> v V.! x == 1) roomA)
    vacantHall = length (filter (\x -> v V.! x == 0) [0, 1])

mutualBlock v = go testHall
  where
    testHall = [3, 5, 7]
    go (x : xs@(_ : _)) = podLeft /= 0 && any f xs || go xs
      where
        podLeft = v V.! x
        roomLeft = podLeft - 1
        hallLeft = roomLeft * 2 + 2
        f y = podRight /= 0 && y < hallLeft && hallRight < x
          where
            podRight = v V.! y
            roomRight = podRight - 1
            hallRight = roomRight * 2 + 2
    go _ = False

calcHue v = hallway + room
  where
    hallway =
      sum
        [ (length (path h (room * 2 + 2)) - 1) * (10 ^ (pod - 1))
          | h <- hallwayIndex,
            let pod = v V.! h,
            pod /= 0,
            let room = pod - 1
        ]
    room =
      sum
        [ moveIn + moveOut
          | r <- [0 .. 3],
            not (availRoom v r),
            s <- [0 .. d - 1],
            let pod = v V.! (11 + r * d + s),
            pod /= r + 1,
            let moveIn = (s + 1) * (10 ^ r),
            let targetRoom = pod - 1,
            let p = path (r * 2 + 2) (targetRoom * 2 + 2),
            let moveOut = if pod == 0 then 0 else (length p + s) * (10 ^ (pod - 1))
        ]
    d = depth v

fastComplete :: Vector Int -> Bool
fastComplete = V.all (== 0) . V.take 11

toInt = V.foldl' (\acc x -> acc * 5 + x) 0

aStar v = go IS.empty (Q.singleton 0 (0, v))
  where
    go _ Empty = Nothing
    go s ((_, x) :< xs)
      | complete (snd x) = Just $ fst x
      | xInt `IS.member` s = go s xs
      | not $ null hToR = go (IS.insert xInt s) (Q.fromList hToR <> xs)
      | not $ null rToR = go (IS.insert xInt s) (Q.fromList rToR <> xs)
      | otherwise = go (IS.insert xInt s) (Q.fromList rToH <> xs)
      where
        hToR = hToRChoices x
        rToR = rToRChoices x
        rToH = rToHChoices x
        xInt = toInt (snd x)

day23 :: IO ()
day23 = do
  inputA <- filter ((||) <$> (== '.') <*> isAlpha) <$> (getDataDir >>= (readFile . (++ "/input/input23.txt")))
  let inputB = take 15 inputA <> "DCBADBAC" <> drop 15 inputA
  putStrLn
    . ("day23a: " ++)
    . show
    . aStar
    $ inputParser inputA
  putStrLn
    . ("day23b: " ++)
    . show
    . aStar
    $ inputParser inputB
