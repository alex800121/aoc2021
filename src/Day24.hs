{-# LANGUAGE DeriveFunctor #-}

module Day24 where

import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import MyLib (Parser, signedInteger)
import Text.Megaparsec (choice, eof, parseMaybe, single)
import Text.Megaparsec.Char (space, string)
import Data.SBV

data Instruction a b
  = Inp a
  | Add a b
  | Mul a b
  | Div a b
  | Mod a b
  | Eql a b
  deriving (Show, Eq, Ord, Functor)

data Vars a = R {_w :: !a, _x :: !a, _y :: !a, _z :: !a} deriving (Show, Eq, Ord, Functor)

data Reg = W | X | Y | Z deriving (Show, Eq, Ord)

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

day24 :: IO ()
day24 = do
  input <- mapMaybe (parseMaybe inputParser) . lines <$> readFile "input/input24.txt"
  print input
