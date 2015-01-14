module Test_Interpret where

import Text.Parsec(char, digit, letter, alphaNum, spaces, parse, string)
--import Text.Parsec.Char
import Text.ParserCombinators.Parsec(try)
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Expr(Operator, Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Control.Applicative(Applicative, many, (<$>), (<*>), (<|>), (<*), (<$), (*>))
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
  (Plus  a b) -> eval vt a >>= \aa -> eval vt b >>= \bb -> return $ (+) aa bb
  (Minus a b) -> eval vt a >>= \aa -> eval vt b >>= \bb -> return $ (-) aa bb
  (Mult  a b) -> eval vt a >>= \aa -> eval vt b >>= \bb -> return $ (*) aa bb
                      --(Div   a b) -> eval vt a >>= \aa -> eval vt b >>= \bb -> return $ (/) aa bb
            {-where
              liftVal :: Convert a => (a -> a -> a) -> Value -> Value -> Value
              liftVal f v1 v2 = toValue $ f (fromValue v1) (fromValue v2)
              
              liftVal f (VBool a) (VBool b) = VBool $ f a b
              liftVal f (VFloat a) (VFloat b) = VFloat $ f a b
              liftVal f (VString a) (VString b) = VString $ f a b
              liftVal _ _ _ = VUnit-}

makeStatement :: Statement -> VarTable -> IO VarTable
makeStatement (Exp exp) vt = eval vt exp >> return vt
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
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- join :: Monad m => m (m a) -> m a

interpret :: String -> IO VarTable
interpret s = case parse (many1 statement) "error" s of 
                Left  _ -> error "interpret error" -- TODO: figure out
                Right p -> run   p