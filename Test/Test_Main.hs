import Text.Parsec(char, digit, letter, alphaNum, spaces, parse, string)
--import Text.Parsec.Char
import Text.ParserCombinators.Parsec(try)
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Expr(Operator, Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Control.Applicative(Applicative, many, (<$>), (<*>), (<|>), (<*), (<$), (*>))
--import Control.Monad(join)
import Data.Char
import Data.Functor.Identity(Identity)
import qualified Data.Map.Strict as M

import Test_Parser
import Test_Interpret
import Test_Types

main :: IO()
main = do 
	ex <- interpret example
	print'' ex
	--print $ interpret example

	--mainLoop
  --print' $ parse (many1 statement) "error" example

print' :: Either ParseError [Statement] -> IO()
print' (Right a) = putStrLn $ unlines $ map show a
print' (Left err) = print err

print'' :: VarTable -> IO()
print'' vt = mapM_ print $ M.toList vt

pp x = interpret x >>= print''
rr x = print' $ parse (many1 statement) "error" x

example :: String
example = "$a = 2 * 3 + 1\n$c = 14\nif 1 then $b = 2 else $a = 1\nif 1 then if 3 then $asd = 123 else $a = 1\n$as = 23 $a = 123 23"

ex2 :: String
ex2 = "$a = 1 $b = 3 $d = 5 12 42"

win x = pp x >> putStrLn "\n" >> rr x