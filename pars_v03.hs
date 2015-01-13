import Text.Parsec(char, digit, letter, alphaNum, spaces, parse, string)
--import Text.Parsec.Char
--import Text.ParserCombinators.Parsec(try)
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Expr(Operator, Operator(Infix), Assoc(AssocLeft), buildExpressionParser)
import Control.Applicative(Applicative, many, (<$>), (<*>), (<|>), (<*), (<$), (*>))
--import Control.Monad(join)
--import Data.Char
import Data.Functor.Identity(Identity)
--import Data.Maybe
--import qualified Data.Map as M

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a] 
a <:> b = (:) <$> a <*> b

--pure :: a -> f a
--Lift a value.

--(<*>) :: f (a -> b) -> f a -> f b infixl 4
--Sequential application.

--(*>) :: f a -> f b -> f b infixl 4
--Sequence actions, discarding the value of the first argument.

--(<*) :: f a -> f b -> f a infixl 4
--Sequence actions, discarding the value of the second argument.

--(<|>) :: f a -> f a -> f a infixl 3
--An associative binary operation

-- (<$>) :: Functor f => (a -> b) -> f a -> f b infixl 4 <$>

main :: IO()
main = 
	--print $ interpret example
  print' $ parse (many1 statement) "error" example

print' :: Either ParseError [Statement] -> IO()
print' (Right a) = putStrLn $ unlines $ map show a

number :: Parser Int
number = read <$> many1 digit

negative :: Parser Int
negative = read <$> char '-' <:> many1 digit


data Expression = Val   Int
                | Booly  Bool
                | Var   String
                | Plus  Expression Expression
                | Minus Expression Expression
                | Mult  Expression Expression
                | Div   Expression Expression
                  deriving (Show)

data Statement = Assignment String Expression
               | If Expression Statement (Maybe Statement)
               | Cmd String [String]
                deriving (Show)

data Value  = VBool Bool
            | VFloat Float
            deriving (Show)

class Convert a where
  toValue :: a -> Value
  fromValue :: Value -> a

instance Convert Bool where
  toValue = VBool
  fromValue (VBool a) = a
  fromValue _ = error "Looking for bool but received something else!"

instance Convert Float where
  toValue = VFloat
  fromValue (VFloat a) = a
  fromValue _ = error "Looking for float but received something else!"

stringInSpaces :: String -> Parser String
stringInSpaces str = spaces *> string str <* spaces

assignment :: Parser Statement
assignment = Assignment <$> (stringInSpaces "$" *> variable) <*> (stringInSpaces "=" *> expression)

parseIf :: Parser Statement
parseIf = If <$> (stringInSpaces "if " *> expression) <*> (stringInSpaces "then " *> statement) <*> optionMaybe (string "else " *> statement)

parseCmd :: Parser Statement
parseCmd = Cmd <$> anyString <*> 

anyString :: Parser Expression
anyString = satisfy (not isSpace)

statement :: Parser Statement
statement = parseIf <|> assignment


-- Both of our operators have the same priority
table :: [[Operator String () Identity Expression]]
table =  [[binary "*" Mult, binary "/" Div], [binary "+" Plus, binary "-" Minus]]
        where binary name f = Infix (f <$ stringInSpaces name) AssocLeft
        
expression :: Parser Expression
expression = buildExpressionParser table other
    where other = var <|> val
          var  = Var <$> ((<* spaces) $ letter <:> many alphaNum)
          val  = Val <$> ((<* spaces) $ number <|> negative)

variable :: Parser String
variable = (<* spaces) $ letter <:> many alphaNum
--integer :: Parser Int
--integer = (<* spaces) $ number <|> negative

{-
operator :: Parser (Expression -> Expression -> Expression)
operator = plus <|> minus
    where plus  = symbol '+' *> return Plus
          minus = symbol '-' *> return Minus

expression :: Parser Expression
expression = term `chainl1` operator
    where term = val <|> var
          var  = Var <$> ((<* spaces) $ letter <:> many alphaNum)
          val  = Val <$> ((<* spaces) $ number <|> negative)
-}

example :: String
example = "$a = 2 * 3 + 1\n$c = 14\nif 1 then $b = 2 else $a = 1"
--example2 = "a = 7 - 9 + 3 + 15\nb = a - 9\nc = a + a + b\na = c + b"
