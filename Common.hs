module Common where

type BinOp = String

data Val =
    IntVal Integer
    | FloatVal Float
        deriving (Eq, Ord, Show)

data Exp = 
    ConstExp Val
    | BinOpExp BinOp Exp Exp
        deriving (Eq, Ord, Show)
