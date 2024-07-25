{-# LANGUAGE DeriveFunctor #-}

module Day24 where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad (guard)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.SBV hiding (Interval)
import MyLib (Parser, signedInteger)
import Paths_AOC2021
import Text.Megaparsec (choice, eof, parseMaybe, single)
import Text.Megaparsec.Char (space, string)

data Instruction a b
  = Inp a
  | Op Op a b
  deriving (Show, Eq, Ord, Functor)

data Interval a = Interval {lower :: !a, upper :: !a} deriving (Show, Eq, Ord)

type Ins = [Instruction Reg (Either Reg Int)]

data Vars a = R {_w :: !a, _x :: !a, _y :: !a, _z :: !a} deriving (Show, Eq, Ord, Functor)

data Reg = W | X | Y | Z deriving (Show, Eq, Ord)

data Op = Add | Mul | Div | Mod | Eql deriving (Show, Eq, Ord)

type Input = [SInt64]

inputParser :: Parser (Instruction Reg (Either Reg Int))
inputParser =
  choice
    [ string "inp " >> (Inp <$> reg),
      string "add " >> (Op Add <$> reg <*> intOrReg),
      string "mul " >> (Op Mul <$> reg <*> intOrReg),
      string "div " >> (Op Div <$> reg <*> intOrReg),
      string "mod " >> (Op Mod <$> reg <*> intOrReg),
      string "eql " >> (Op Eql <$> reg <*> intOrReg)
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
  Inp r : ins' -> calcInt (writeToReg r (Interval 1 9) v) ins'
  Op op a b : ins' ->
    let a'@(Interval lowerA upperA) = readFromReg a v
        b'@(Interval lowerB upperB) = f b v
        x = case op of
          Add -> Just $ Interval (lowerA + lowerB) (upperA + upperB)
          Mul -> let l = (*) <$> [lowerA, upperA] <*> [lowerB, upperB] in Just $ Interval (minimum l) (maximum l)
          Eql | lowerA == upperA && lowerA == lowerB && lowerA == upperB -> Just $ Interval 1 1
          Eql | lowerA > upperB || upperA < lowerB -> Just $ Interval 0 0
          Eql -> Just $ Interval 0 1
          Div ->
            if lowerB > 0 || upperB < 0
              then let l = div <$> [lowerA, upperA] <*> [lowerB, upperB] in Just $ Interval (minimum l) (maximum l)
              else Nothing
          Mod -> if lowerA < 0 && lowerB <= 0 then Nothing else Just $ Interval 0 (upperB - 1)
     in maybe False (\y -> calcInt (writeToReg a y v) ins') x
    -- Add a b : ins' ->
    --   let a' = readFromReg a v
    --       b' = f b v
    --    in calcInt (writeToReg a (Interval (lower a' + lower b') (upper a' + upper b')) v) ins'
    -- Mul a b : ins' | Interval a1 a2 <- readFromReg a v, Interval b1 b2 <- f b v ->
    --   let l = (*) <$> [a1 .. a2] <*> [b1 .. b2]
    --    in calcInt (writeToReg a (Interval (minimum l) (maximum l)) v) ins'
  where
    f b v = case b of
      Left r -> readFromReg r v
      Right i -> Interval i i

intervalify :: Vars a -> Vars (Interval a)
intervalify (R w x y z) = R (f w) (f x) (f y) (f z)
  where
    f a = Interval a a

readIns' :: [Int] -> Vars Int -> Ins -> [[Int]]
readIns' inputs v ins = case ins of
  [] | _z v == 0 -> pure []
  [] -> empty
  Inp r : ins' -> do
    i <- inputs
    let v' = writeToReg r i v
    if calcInt (intervalify v') ins'
      then (i :) <$> readIns' inputs v' ins'
      else empty
  Op op a b : ins' ->
    let a' = readFromReg a v
        b' = f b v
        (c, g) = case op of
          Add -> (True, (+))
          Mul -> (True, (*))
          Eql -> (True, \x y -> if x == y then 1 else 0)
          Div -> (b' /= 0, div)
          Mod -> (a' >= 0 && b' > 0, mod)
        v' = writeToReg a (g a' b') v
     in if c && calcInt (intervalify v') ins' then readIns' inputs v' ins' else empty
  where
    f b v = case b of
      Left r -> readFromReg r v
      Right i -> fromIntegral i

readIns :: Input -> Ins -> Vars SInt64
readIns = go (R 0 0 0 0)
  where
    go v _ [] = v
    go v (x : xs) (Inp reg : ins) = go (writeToReg reg x v) xs ins
    go v xs (Op op a b : ins) =
      let a' = readFromReg a v
          b' = f b v
          g = case op of
            Add -> (+)
            Mul -> (*)
            Div -> sDiv
            Mod -> sMod
            Eql -> \x y -> ite (x .== y) 1 0
       in go (writeToReg a (g a' b') v) xs ins
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
  input <- mapMaybe (parseMaybe inputParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  -- x <- sola maximize input
  -- y <- solb minimize input
  -- putStrLn . ("day24a: \n" ++) $ show x
  -- putStrLn . ("day24a: \n" ++) $ show y
  putStrLn $ ("day24a: " ++) $ concatMap show $ head $ readIns' [9, 8 .. 1] (R 0 0 0 0) input
  putStrLn $ ("day24b: " ++) $ concatMap show $ head $ readIns' [1 .. 9] (R 0 0 0 0) input

-- print $ calcInt (intervalify (R 0 0 0 0)) input
