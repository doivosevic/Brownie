module Test_Parser where

import Text.Parsec(char, digit, letter, alphaNum, spaces, parse, string)
--import Text.Parsec.Char
import Text.ParserCombinators.Parsec(try)
import Text.ParserCombinators.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Expr(Operator, Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Control.Applicative(Applicative, many, (<$>), (<*>), (<|>), (<*), (<$), (*>))
--import Control.Monad(join)
import Data.Char
import Data.Functor.Identity(Identity)
--import Data.Maybe
--import qualified Data.Map as M

import Test_Types

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

stringInSpaces :: String -> Parser String
stringInSpaces str = spaces *> string str <* spaces

assignment :: Parser Statement
assignment = Assignment <$> (stringInSpaces "$" *> variable) <*> (stringInSpaces "=" *> expression)

parseIf :: Parser Statement
parseIf = If <$> (stringInSpaces "if " *> expression) <*> (stringInSpaces "then " *> statement)

parseIfElse :: Parser Statement
parseIfElse = IfElse <$> (stringInSpaces "if " *> expression) <*> (stringInSpaces "then " *> statement) <*> (stringInSpaces "else " *> statement)

statement :: Parser Statement
statement = try parseIfElse <|> parseIf <|> assignment <|> nakedExpression

nakedExpression :: Parser Statement
nakedExpression = Exp <$> expression

table :: [[Operator String () Identity Expression]]
table =  [[binary "*" Mult], [binary "+" Plus, binary "-" Minus]]
        where binary name f = Infix (f <$ stringInSpaces name) AssocLeft
        --, binary "/" Div MISSING!! TODO...

expression :: Parser Expression
expression = buildExpressionParser table other
    where other = var <|> val
          var  = Var <$> ((<* spaces) $ letter <:> many alphaNum)
          val  = Val <$> VFloat <$> ((<* spaces) $ number <|> negative)
--          stri = Val <$> VString <$> (between (symbol "*") (symbol "*") (many1 anyChar))

variable :: Parser String
variable = (<* spaces) $ letter <:> many alphaNum

number :: Parser Float
number = read <$> many1 digit

negative :: Parser Float
negative = read <$> char '-' <:> many1 digit