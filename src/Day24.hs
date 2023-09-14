{-# LANGUAGE DeriveFunctor #-}

module Day24 where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.SBV
import MyLib (Parser, signedInteger)
import Text.Megaparsec (choice, eof, parseMaybe, single)
import Text.Megaparsec.Char (space, string)
import Data.List (foldl')

data Instruction a b
  = Inp a
  | Add a b
  | Mul a b
  | Div a b
  | Mod a b
  | Eql a b
  deriving (Show, Eq, Ord, Functor)

type Ins = [Instruction Reg (Either Reg Int)]

data Vars a = R {_w :: !a, _x :: !a, _y :: !a, _z :: !a} deriving (Show, Eq, Ord, Functor)

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

sol i = optimize Lexicographic $ do
  v <- mkFreeVars 14
  constrain $ valid v i
  maximize "value" $ toNum v

day24 :: IO ()
day24 = do
  input <- mapMaybe (parseMaybe inputParser) . lines <$> readFile "input/input24.txt"
  print =<< sol input
