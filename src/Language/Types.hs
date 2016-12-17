{-# LANGUAGE FlexibleInstances #-}
module Language.Types where


import qualified Data.Map.Strict as M

type InterpreterState = (VarTable, CWD)

type CWD = String

data Expression = Val   Value
                | Var   String
                | Plus  Expression Expression
                | Minus Expression Expression
                | Mult  Expression Expression
                | GTe    Expression Expression
                | GEe    Expression Expression
                | LTe    Expression Expression
                | LEe    Expression Expression
                | EQe    Expression Expression
                | Cmd   String [String]
                  deriving (Show)
                --TODO : | Div   Expression Expression

data Statement = Assignment String Expression
               | If Expression Statement
               | IfElse Expression Statement Statement
               | While Expression Statement
               | Exp Expression
               | Seq [Statement]
                deriving (Show)

data Value  = VBool Bool
            | VDouble Double
            | VString String
            | VUnit
            deriving (Show)

type VarTable = M.Map String Value

instance Eq Value where
  (==) (VDouble a) (VDouble b) = a == b
  (==) (VBool a) (VBool b) = a == b
  (==) (VString a) (VString b) = a == b
  (==) _ _ = error "Invalid equality test!!"

instance Ord Value where
  compare (VDouble a) (VDouble b) = compare a b
  compare (VBool  a) (VBool  b) = compare a b
  compare (VString a) (VString b) = compare a b
  compare _ _ = error "Invalid comparison!!"

isTrue :: Value -> Bool
isTrue (VDouble a)
  | a == 0 = False
  | a == 1 = True
  | otherwise = error "The number is neither 0 nor 1"
isTrue (VBool a) = a
isTrue (VString str) = error "String can not be true of false"

instance Num Value where
    (+) (VDouble a) (VDouble b) = VDouble $ a + b
    (+) _ _ = error "Invalid operation on val (+)"
    (*) (VDouble a) (VDouble b) = VDouble $ a * b
    (*) _ _ = error "Invalid operation on val (*)"
    abs (VDouble a)      = VDouble $ abs a
    abs _ = error "Invalid operation on val (abs)"
    signum (VDouble a)   = VDouble $ signum a
    signum _ = error "Invalid operation on val (signum)"
    negate (VDouble a) = VDouble (-a)
    negate (VBool a) = VBool (not a)
    fromInteger a = VDouble $ fromInteger a
