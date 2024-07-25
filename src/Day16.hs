module Day16 where

import Data.Bifunctor (Bifunctor (..))
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import MyLib
import Paths_AOC2021

data Packet op payload
  = Lit {version :: Int, payLoad :: payload}
  | Op {version :: Int, op :: op, subPackets :: [Packet op payload]}
  deriving (Show, Eq, Ord)

instance Functor (Packet op) where
  fmap f (Lit v p) = Lit v (f p)
  fmap f (Op v op sp) = Op v op (fmap f <$> sp)

interpretPacket :: Packet Int Int -> Int
interpretPacket (Lit _ p) = p
interpretPacket (Op _ op sp) = case op of
  0 -> sum p
  1 -> product p
  2 -> minimum p
  3 -> maximum p
  5 -> fromEnum (head p > p !! 1)
  6 -> fromEnum (head p < p !! 1)
  7 -> fromEnum (head p == p !! 1)
  where
    p = map interpretPacket sp

bitToInt = baseNToInt 2

parsePacket :: String -> (String, Packet Int String)
parsePacket input
  | op == 4 = let (rest, pl) = parsePayload input2 in (rest, Lit ver pl)
  | i == '0' = (rest0, Op ver op (f0 input50))
  | i == '1' = Op ver op <$> f1 len1 input41
  | otherwise = error $ show i
  where
    (v, input1) = splitAt 3 input
    ver = bitToInt v
    (t, input2) = splitAt 3 input1
    op = bitToInt t
    (i : input3) = input2
    (l0, input40) = splitAt 15 input3
    len0 = bitToInt l0
    (input50, rest0) = splitAt len0 input40
    f0 s
      | null s = []
      | otherwise = let (s', p) = parsePacket s in p : f0 s'
    (l1, input41) = splitAt 11 input3
    len1 = bitToInt l1
    f1 0 s = (s, [])
    f1 n s = let (s', p) = parsePacket s in fmap (p :) $ f1 (n - 1) s'

parsePayload :: String -> (String, String)
parsePayload ('0' : xs) = swap $ splitAt 4 xs
parsePayload ('1' : xs) = let (y, ys) = splitAt 4 xs in (y <>) <$> parsePayload ys

addVersion :: Packet op payload -> Int
addVersion (Lit v p) = v
addVersion (Op v _ p) = v + sum (map addVersion p)

day16 :: IO ()
day16 = do
  input <- concat . mapMaybe hexTo4Bits <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  let packet = snd $ parsePacket input
  putStrLn $ ("day16a: " ++) $ show $ addVersion packet
  putStrLn $ ("day16b: " ++) $ show $ interpretPacket $ bitToInt <$> packet
