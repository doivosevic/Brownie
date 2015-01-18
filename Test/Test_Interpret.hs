module Test_Interpret where

import Text.Parsec.Expr
import Text.Parsec.String
import Text.ParserCombinators.Parsec

import Control.Applicative hiding ((<|>),many)
import Control.Monad(join)
import Data.Char
import Data.Functor.Identity(Identity)
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

import Test_Parser
import Test_Types

eval :: InterpreterState -> Expression -> IO (Value, CWD)
eval state@(vt, cwd) e = case e of
  (Cmd name args) -> execute state name args
  (Val   v)   -> return (v, cwd)
  (Var   v)   -> return (fromJust $ M.lookup v $ vt, cwd)
  (Plus  a b) -> fly (+) a b
  (Minus a b) -> fly (-) a b
  (Mult  a b) -> fly (*) a b
  (GTe    a b) -> fly2 (>) a b
  (GEe    a b) -> fly2 (>=) a b
  (LTe    a b) -> fly2 (<) a b
  (LEe    a b) -> fly2 (<=) a b
  (EQe    a b) -> fly2 (==) a b
  where 
    fly :: (Value -> Value -> Value) -> Expression -> Expression -> IO (Value, CWD)
    fly f a b  = eval state a >>= \(aa, cwd1) -> eval state b >>= \(bb, cwd2) -> return (f aa bb, cwd2)
    fly2 f a b = eval state a >>= \(aa, cwd1) -> eval state b >>= \(bb, cwd2) -> return (VBool (f aa bb), cwd2)

makeStatement :: Statement -> InterpreterState -> IO InterpreterState
makeStatement (Exp exp) state@(vt, cwd) = eval state exp >>= \(val, newCwd) -> print val >> return (vt, newCwd)
makeStatement (If cond stmt) state@(vt, cwd) = do
    (c, newCwd) <- eval state cond
    case c of
      VBool True -> makeStatement stmt (vt, newCwd)
      VDouble f   -> if f /= 0 then makeStatement stmt (vt, newCwd) else return (vt, newCwd)
      _          -> return (vt, newCwd)
makeStatement (IfElse cond stmt1 stmt2) state@(vt, cwd) = do
    (c, newCwd) <- eval state cond
    case c of
      VBool b      -> if b      then makeStatement stmt1 (vt, newCwd) else makeStatement stmt2 (vt, newCwd)
      VDouble f    -> if f /= 0 then makeStatement stmt1 (vt, newCwd) else makeStatement stmt2 (vt, newCwd)
      _            -> makeStatement stmt2 (vt, newCwd)
makeStatement (Assignment var exp) state@(vt, cwd) = eval state exp >>= \(res, newCwd) -> return (M.insert var res vt, newCwd)

run :: InterpreterState -> [Statement] -> IO InterpreterState
run state (s:stat) = makeStatement s state >>= flip run stat
run state [] = return state

interpret :: InterpreterState -> String -> IO InterpreterState
interpret state s = case parse (many1 statement) "error" s of 
                Left  e -> error $ show e -- TODO: figure out
                Right p -> run state p

execute :: InterpreterState -> String -> [String] -> IO (Value, CWD)
execute (vt, cwd) cmdName cmdArgs = case map toUpper cmdName of
  "CD"  -> cmdCD cmdArgs cwd
  _     -> return (VDouble 1.0, cwd)

cmdCD cmdArgs cwd = case head cmdArgs of
  ".." -> case elemIndices '/' cwd of
            []  -> return (VDouble 0, cwd)
            arr -> return (VDouble 1, take (last arr) cwd)
  _    -> return (VDouble 1, cwd)