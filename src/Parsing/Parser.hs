module Parsing.Parser where
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import Base.Types

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Double
float = lexeme L.float

--stringLiteral :: Parser String
--stringLiteral = lexeme L.

semi :: Parser String
semi = symbol ";"

reservedOp :: String -> Parser ()
reservedOp w = string w *> sc

rws = ["if", "then", "else", "fi", "while", "do"
      ,"od", "true", "false", "not", "and", "or"
      ,"+", "-", "*", "=", ";", "$"
      , ">", "<", "<=", ">=", "==", "\n"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

variable :: Parser String
variable        = reservedOp "$" *> identifier

--stringLiteral   = Token.stringLiteral lexer

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
parseSeq = Seq <$> between (symbol "{") (symbol "}") (some statement)

nakedExpression :: Parser Statement
nakedExpression = Exp <$> expression <* oneOf ";\n"

statement :: Parser Statement
statement = parseSeq <|> parseWhile <|> try parseIfElse <|> parseIf <|> try assignment <|> nakedExpression

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

--table :: [[Operator String () Identity Expression]]
table :: [[Operator Parser Expression]]
table =     [[binary "*" Mult]
            ,[binary "+" Plus, binary "-" Minus]
            ,[binary ">" GTe, binary ">=" GEe, binary "<" LTe, binary "<=" LEe]
            ,[binary "==" EQe]]
    where binary name f = InfixL (f <$ reservedOp name)
        --, binary "/" Div MISSING!! TODO...

-- ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
-- ::::::::::::::::::::::::::::: EXPRESSION PARSERS :::::::::::::::::::::::::::
expression :: Parser Expression
expression = makeExprParser other table
    where other = cmd  <|> val <|> var-- <|> stri

var  = Var <$> variable
val  = Val . VDouble <$> (try float <|> fromInteger <$> integer)
--stri = Val . VString <$> stringLiteral

cmd = do
    name <- identifier :: Parser String
    args <- many $ noneOf ";\n"
    return $ Cmd name $ words args
