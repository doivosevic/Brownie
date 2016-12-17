module Parsing.Parser where

import Control.Monad
import Control.Applicative hiding ((<|>),many)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Data.Char
import Data.Functor.Identity(Identity)

import Language.Types

languageDef =
    emptyDef { Token.commentStart       = "/*"
             , Token.commentEnd         = "*/"
             , Token.commentLine        = "//"
             , Token.identStart         = letter
             , Token.identLetter        = alphaNum
             , Token.reservedNames      = [ "if"
                                          , "then"
                                          , "else"
                                          , "fi"
                                          , "while"
                                          , "do"
                                          , "od"
                                          , "true"
                                          , "false"
                                          , "not"
                                          , "and"
                                          , "or" 
                                          ]
             , Token.reservedOpNames    = [ "+", "-", "*", "=", ";", "$"
                                          , ">", "<", "<=", ">=", "=="
                                          , "and", "or", "not" , "\n"
                                          ]
            }

lexer = Token.makeTokenParser languageDef

identifier      = Token.identifier  lexer
variable        = reservedOp "$" *> identifier

reserved        = Token.reserved    lexer
reservedOp      = Token.reservedOp  lexer
parens          = Token.parens      lexer
stringLiteral   = Token.stringLiteral lexer

float           = Token.float       lexer
integer         = Token.integer     lexer
semi            = Token.semi        lexer
whiteSpace      = Token.whiteSpace  lexer
symbol          = Token.symbol      lexer

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- :::::::::::::::::::::::::::::: STATEMENT PARSERS :::::::::::::::::::::::::::
assignment :: Parser Statement
assignment = Assignment <$> variable <*> (reservedOp "=" *> expression <* reservedOp ";")

parseIf :: Parser Statement
parseIf = If <$> (reservedOp "if" *> expression) 
                <*> (reservedOp "then" *> statement <* reservedOp "fi")

parseIfElse :: Parser Statement
parseIfElse = IfElse <$> (reservedOp "if" *> expression) 
                        <*> (reservedOp "then" *> statement) 
                        <*> (reservedOp "else" *> statement <* reservedOp "fi")

parseWhile :: Parser Statement
parseWhile = While <$> (reservedOp "while" *> expression)
                        <*> (reservedOp "do" *> statement <* reservedOp "od")

parseSeq :: Parser Statement
parseSeq = Seq <$> between (symbol "{") (symbol "}") (many1 statement)

nakedExpression :: Parser Statement
nakedExpression = Exp <$> expression <* oneOf ";\n"

statement :: Parser Statement
statement = parseSeq <|> parseWhile <|> try parseIfElse <|> parseIf <|> try assignment <|> nakedExpression

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

--table :: [[Operator String () Identity Expression]]
table :: [[Operator Char st Expression]]
table =     [[binary "*" Mult]
            ,[binary "+" Plus, binary "-" Minus]
            ,[binary ">" GTe, binary ">=" GEe, binary "<" LTe, binary "<=" LEe]
            ,[binary "==" EQe]]
    where binary name f = Infix (f <$ reservedOp name) AssocLeft
        --, binary "/" Div MISSING!! TODO...

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- ::::::::::::::::::::::::::::: EXPRESSION PARSERS :::::::::::::::::::::::::::
expression :: Parser Expression
expression = buildExpressionParser table other
    where other = cmd  <|> val <|> stri <|> var

var  = Var <$> variable
val  = Val <$> VDouble <$> (try float <|> fromInteger <$> integer)
stri = Val <$> VString <$> stringLiteral

cmd = do
    name <- identifier :: Parser String
    args <- many $ noneOf ";\n"
    return $ Cmd name $ words $ args
