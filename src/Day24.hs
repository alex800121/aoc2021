module Day24 where

data Instruction a b
  = Inp a
  | Add a b
  | Mul a b
  | Div a b
  | Mod a b
  | Eql a b
  deriving (Show, Eq, Ord)

data Vars a = R {_w :: a, _x :: a, _y :: a, _z :: a} deriving (Show, Eq, Ord)

day24 :: IO ()
day24 = return ()
