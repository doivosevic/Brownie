{-# LANGUAGE FlexibleInstances #-}

module Test_Types where

import qualified Data.Map.Strict as M

data Expression = Val   Value
                | Var   String
                | Plus  Expression Expression
                | Minus Expression Expression
                | Mult  Expression Expression
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

instance Num Value where
    (+) (VFloat a) (VFloat b) = VFloat $ a + b
    (+) _ _ = error "Invalid operation on val (+)"
    (*) (VFloat a) (VFloat b) = VFloat $ a * b
    (*) _ _ = error "Invalid operation on val (*)"
    abs (VFloat a)      = VFloat $ abs a
    abs _ = error "Invalid operation on val (abs)"
    signum (VFloat a)   = VFloat $ signum a
    signum _ = error "Invalid operation on val (signum)"
--    fromInteger (VFloat a) = error "No bueno" --VFloat $ fromInteger a
    fromInteger _ = error "Invalid operation on val (fromInteger)"

{-instance Convert Bool where
  toValue = VBool
  fromValue (VBool a) = a
  fromValue _ = error "Looking for bool but received something else!"

instance Convert Float where
  toValue = VFloat
  fromValue (VFloat a) = a
  fromValue _ = error "Looking for float but received something else!"

instance Convert [Char] where
  toValue = VString
  fromValue (VString a) = a
  fromValue _ = error "Looking for string but received something else!"


class Convert a where
    toValue :: a -> Value
    fromValue :: Value -> a-}