module Test_Interpret where

import Text.Parsec.Expr
import Text.Parsec.String
import Text.ParserCombinators.Parsec

import Control.Applicative hiding ((<|>),many)
import Control.Monad(join)
import Data.Char
import Data.Functor.Identity(Identity)
import Data.Maybe
import qualified Data.Map.Strict as M

import Test_Parser
import Test_Types

eval :: VarTable -> Expression -> IO Value
eval vt e = case e of 
  (Val   v)   -> return v
  (Var   v)   -> return $ fromJust $ M.lookup v vt
  (Plus  a b) -> fly (+) a b
  (Minus a b) -> fly (-) a b
  (Mult  a b) -> fly (*) a b
  (GTe    a b) -> fly2 (>) a b
  (GEe    a b) -> fly2 (>=) a b
  (LTe    a b) -> fly2 (<) a b
  (LEe    a b) -> fly2 (<=) a b
  (EQe    a b) -> fly2 (==) a b
  where 
    fly f a b = eval vt a >>= \aa -> eval vt b >>= \bb -> return $ f aa bb
    fly2 f a b = VBool <$> fly f a b

makeStatement :: Statement -> VarTable -> IO VarTable
makeStatement (Exp exp) vt = eval vt exp >>= print >> return vt
makeStatement (If cond stmt) vt = do
    c <- eval vt cond
    case c of
      VBool True -> makeStatement stmt vt
      VFloat f   -> if f /= 0 then makeStatement stmt vt else return vt
      _          -> return vt
makeStatement (IfElse cond stmt1 stmt2) vt = do
    c <- eval vt cond
    case c of
      VBool b     -> if b      then makeStatement stmt1 vt else makeStatement stmt2 vt
      VFloat f    -> if f /= 0 then makeStatement stmt1 vt else makeStatement stmt2 vt
      _           -> makeStatement stmt2 vt
makeStatement (Assignment var exp) vt = eval vt exp >>= \res -> return (M.insert var res vt)

run :: [Statement] -> IO VarTable
run = run' M.empty
    where 
      run' vt (s:stat) = makeStatement s vt >>= flip run' stat
      run' vt [] = return vt

interpret :: String -> IO VarTable
interpret s = case parse (many1 statement) "error" s of 
                Left  e -> error $ show e -- TODO: figure out
                Right p -> run   p