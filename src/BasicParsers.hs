module BasicParsers where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "fork"
                                      , "join"
                                      , "print"
                                      , "while"
                                      , "func"
                                      , "int"
                                      , "bool"
                                      , "void"
                                      , "?"
                                      , "if"
                                      , "nop"
                                      , "ya"
                                      , "nu"
                                      , "global"
                                      , "{", "}"
                                      , "return"
                                      , "&"
                                      ]
            , Token.reservedOpNames = [ "+" , "*", "-"
                                      , "<", ">", "==", ">=", "<="
                                      , "="
                                      , "++"
                                      , "--"
                                      , "+="
                                      , "-="
                                      ]
            }




lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer


integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whitespace :: Parser ()
whitespace = Token.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

comma :: Parser String
comma = Token.comma lexer

semi :: Parser String
semi = Token.semi lexer
semi_test = parse semi "" ";"

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semisep :: Parser a -> Parser [a]
semisep = Token.semiSep lexer
