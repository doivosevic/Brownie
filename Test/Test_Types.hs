{-# LANGUAGE FlexibleInstances #-}

module Test_Types where

import qualified Data.Map.Strict as M

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
               | Exp Expression
                deriving (Show)

data Value  = VBool Bool
            | VFloat Float
            | VString String
            | VUnit
            deriving (Show)

type VarTable = M.Map String Value

instance Eq Value where
  (==) (VFloat a) (VFloat b) = a == b
  (==) (VBool a) (VBool b) = a == b
  (==) (VString a) (VString b) = a == b
  (==) _ _ = error "Invalid equality test!!"

instance Ord Value where
  compare (VFloat a) (VFloat b) = compare a b
  compare (VBool  a) (VBool  b) = compare a b
  compare (VString a) (VString b) = compare a b
  compare _ _ = error "Invalid comparison!!"

instance Num Value where
    (+) (VFloat a) (VFloat b) = VFloat $ a + b
    (+) _ _ = error "Invalid operation on val (+)"
    (*) (VFloat a) (VFloat b) = VFloat $ a * b
    (*) _ _ = error "Invalid operation on val (*)"
    abs (VFloat a)      = VFloat $ abs a
    abs _ = error "Invalid operation on val (abs)"
    signum (VFloat a)   = VFloat $ signum a
    signum _ = error "Invalid operation on val (signum)"
    fromInteger _ = error "Invalid operation on val (fromInteger)"