module Test_Parser where

import Text.Parsec.String
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>),many)
import Data.Char
import Data.Functor.Identity(Identity)

import Test_Types

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

sisp :: String -> Parser String
sisp str = spaces *> string str <* spaces

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- :::::::::::::::::::::::::::::: STATEMENT PARSERS :::::::::::::::::::::::::
assignment :: Parser Statement
assignment = Assignment <$> (sisp "$" *> varP) <*> (sisp "=" *> expression)

parseIf :: Parser Statement
parseIf = If <$> (sisp "if " *> expression) <*> (sisp "then " *> statement <* sisp "fi")

parseIfElse :: Parser Statement
parseIfElse = IfElse <$> (sisp "if " *> expression) <*> (sisp "then " *> statement) <*> (sisp "else " *> statement <* sisp "fi")

nakedExpression :: Parser Statement
nakedExpression = Exp <$> expression

statement :: Parser Statement
statement = try parseIfElse <|> parseIf <|> assignment <|> nakedExpression

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

table :: [[Operator String () Identity Expression]]
table =  	[[binary "*" Mult]
			,[binary "+" Plus, binary "-" Minus]
			,[binary ">" GTe, binary ">=" GEe, binary "<" LTe, binary "<=" LEe]
			,[binary "==" EQe]]
	where binary name f = Infix (f <$ sisp name) AssocLeft
        --, binary "/" Div MISSING!! TODO...

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- ::::::::::::::::::::::::::::: EXPRESSION PARSERS :::::::::::::::::::::::::
expression :: Parser Expression
expression = buildExpressionParser table other
    where other = var <|> val <|> stri
          var  = Var <$> varP
          val  = Val <$> VFloat <$> (posP <|> negP)
          stri = Val <$> VString <$> strP

varP :: Parser String
varP = (<* spaces) $ letter <:> many alphaNum

posP :: Parser Float
posP = (<* spaces) $ read <$> many1 digit

negP :: Parser Float
negP = (<* spaces) $ read <$> char '-' <:> many1 digit

strP :: Parser String
strP = between (char '"' :: Parser Char) (char '"' :: Parser Char) (many1 $ noneOf "\"")