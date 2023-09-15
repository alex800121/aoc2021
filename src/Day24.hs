{-# LANGUAGE DeriveFunctor #-}

module Day24 where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad (guard)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.SBV hiding (Interval)
import MyLib (Parser, signedInteger)
import Text.Megaparsec (choice, eof, parseMaybe, single)
import Text.Megaparsec.Char (space, string)

data Instruction a b
  = Inp a
  | Add a b
  | Mul a b
  | Div a b
  | Mod a b
  | Eql a b
  deriving (Show, Eq, Ord, Functor)

data Interval a = Interval {lower :: a, upper :: a} deriving (Show, Eq, Ord)

type Ins = [Instruction Reg (Either Reg Int)]

data Vars a = R {_w :: a, _x :: a, _y :: a, _z :: a} deriving (Show, Eq, Ord, Functor)

data Reg = W | X | Y | Z deriving (Show, Eq, Ord)

type Input = [SInt64]

inputParser :: Parser (Instruction Reg (Either Reg Int))
inputParser =
  choice
    [ string "inp " >> (Inp <$> reg),
      string "add " >> (Add <$> reg <*> intOrReg),
      string "mul " >> (Mul <$> reg <*> intOrReg),
      string "div " >> (Div <$> reg <*> intOrReg),
      string "mod " >> (Mod <$> reg <*> intOrReg),
      string "eql " >> (Eql <$> reg <*> intOrReg)
    ]
  where
    reg =
      choice
        [ single 'w' >> return W,
          single 'x' >> return X,
          single 'y' >> return Y,
          single 'z' >> return Z
        ]
        <* space
    intOrReg = (Left <$> reg) <|> (Right <$> signedInteger)

writeToReg :: Reg -> a -> Vars a -> Vars a
writeToReg r a v = case r of
  W -> v {_w = a}
  X -> v {_x = a}
  Y -> v {_y = a}
  Z -> v {_z = a}

readFromReg :: Reg -> Vars a -> a
readFromReg r = case r of
  W -> _w
  X -> _x
  Y -> _y
  Z -> _z

calcInt :: Vars (Interval Int) -> Ins -> Bool
calcInt v ins = case ins of
  [] -> let z = _z v in 0 <= upper z && 0 >= lower z
  Inp r : ins' -> calcInt (writeToReg r (Interval 1 9) v) ins
  Add a b : ins' ->
    let a' = readFromReg a v
        b' = f b v
     in calcInt (writeToReg a (Interval (lower a' + lower b') (upper a' + upper b')) v) ins'
  Mul a b : ins' ->
    let a' = readFromReg a v
        Interval b1 b2 = f b v
     in undefined
  where
    f b v = case b of
      Left r -> readFromReg r v
      Right i -> Interval i i

readIns' :: [Int] -> Vars Int -> Ins -> [[Int]]
readIns' inputs v ins = case ins of
  [] | _z v == 0 -> pure []
  [] -> empty
  Inp r : ins' -> do
    i <- inputs
    (i :) <$> readIns' inputs (writeToReg r i v) ins'
  Add a b : ins' -> readIns' inputs (writeToReg a (readFromReg a v + f b v) v) ins'
  Mul a b : ins' -> readIns' inputs (writeToReg a (readFromReg a v * f b v) v) ins'
  Eql a b : ins' -> readIns' inputs (writeToReg a (if readFromReg a v == f b v then 1 else 0) v) ins'
  Div a b : ins' -> do
    let b' = f b v
    guard $ b' /= 0
    let a' = readFromReg a v
    readIns' inputs (writeToReg a (a' `div` b') v) ins'
  Mod a b : ins' -> do
    let b' = f b v
        a' = readFromReg a v
    guard $ a' >= 0 && b' > 0
    readIns' inputs (writeToReg a (a' `mod` b') v) ins'
  where
    f b v = case b of
      Left r -> readFromReg r v
      Right i -> fromIntegral i

readIns :: Input -> Ins -> Vars SInt64
readIns = go (R 0 0 0 0)
  where
    go v _ [] = v
    go v (x : xs) (Inp reg : ins) = go (writeToReg reg x v) xs ins
    go v xs (Add a b : ins) = go (writeToReg a (readFromReg a v + f b v) v) xs ins
    go v xs (Mul a b : ins) = go (writeToReg a (readFromReg a v * f b v) v) xs ins
    go v xs (Div a b : ins) = go (writeToReg a (readFromReg a v `sDiv` f b v) v) xs ins
    go v xs (Mod a b : ins) = go (writeToReg a (readFromReg a v `sMod` f b v) v) xs ins
    go v xs (Eql a b : ins) = go (writeToReg a (ite (readFromReg a v .== f b v) 1 0) v) xs ins
    f b v = case b of
      Left r -> readFromReg r v
      Right i -> fromIntegral i

toNum :: Input -> SInt64
toNum = foldl' (\acc -> (+ acc * 10)) 0

valid :: Input -> Ins -> SBool
valid v i = sAll (\x -> x .<= 9 .&& x .>= 1) v .&& (0 .== _z (readIns v i))

sola :: (String -> SInt64 -> Symbolic ()) -> Ins -> IO OptimizeResult
sola f i = optimize Lexicographic $ do
  v <- mkFreeVars 14
  constrain $ valid v i
  f "value" $ toNum v

solb :: (String -> SInt64 -> Symbolic ()) -> Ins -> IO OptimizeResult
solb f i = optimize Lexicographic $ do
  v <- mkFreeVars 14
  constrain $ valid v i
  f "value" $ toNum v

day24 :: IO ()
day24 = do
  input <- mapMaybe (parseMaybe inputParser) . lines <$> readFile "input/input24.txt"
  print $ head $ readIns' [9, 8 .. 1] (R 0 0 0 0) input

-- x <- sola maximize input
-- y <- solb minimize input
-- putStrLn . ("day24a: \n" ++) $ show x
-- putStrLn . ("day24a: \n" ++) $ show y
