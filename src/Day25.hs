module Day25 where

import Data.Bits (Bits (..))
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Strict qualified as S
import Data.WideWord (Word256)
import MyLib (drawArray)
import Paths_AOC2021 (getDataDir)

type Index = (Int, Int)

inputParser i = (w, e, s)
  where
    ss = lines i
    e = V.fromList $ map (f '>' 0 (0 :: Word256)) ss
    s = V.fromList $ map (f 'v' 0 (0 :: Word256)) ss
    w = length (head ss)
    f a i c [] = c
    f a i c (b : bs) = f a (succ i) (if a == b then c `setBit` (w - 1 - i) else c) bs

east w = V.map (\x -> (if x `testBit` 0 then (`setBit` (w - 1)) else id) (x `shiftR` 1))

west w = V.map (\x -> (if x `testBit` (w - 1) then (`clearBit` w) . (`setBit` 0) else id) (x `shiftL` 1))

south v = V.cons (V.last v) (V.init v)

north v = V.snoc (V.tail v) (V.head v)

bitMask a b = a .&. complement b

moveEast (w, e, s) = (w, e', s)
  where
    proposed = east w e
    masked = V.zipWith bitMask proposed (V.zipWith (.|.) e s)
    masked' = west w masked
    e' = V.zipWith3 (\a b c -> bitMask (a .|. b) c) e masked masked'

moveSouth (w, e, s) = (w, e, s')
  where
    proposed = south s
    masked = V.zipWith bitMask proposed (V.zipWith (.|.) e s)
    masked' = north masked
    s' = V.zipWith3 (\a b c -> bitMask (a .|. b) c) s masked masked'

move :: (Int, Vector Word256, Vector Word256) -> (Int, Vector Word256, Vector Word256)
move = moveSouth . moveEast

printer (w, e, s) = unlines (zipWith (zipWith g) ve vs)
  where
    g b c
      | b = '>'
      | c = 'v'
      | otherwise = '.'
    f i x
      | i < 0 = []
      | otherwise = (x `testBit` i) : f (pred i) x
    ve = map (f (w - 1)) $ V.toList e
    vs = map (f (w - 1)) $ V.toList s

stable f x = go 1 x
  where
    go n x
      | x == x' = n
      | otherwise = go (succ n) x'
      where
        x' = f x

day25 :: IO ()
day25 = do
  input <- inputParser <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  putStrLn
    . ("day25a: " ++)
    . show
    $ stable move input
  putStrLn "Merry Christmas!!!"
