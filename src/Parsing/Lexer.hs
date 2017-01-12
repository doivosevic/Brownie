module Parsing.Lexer (lexer) where

import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec

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
