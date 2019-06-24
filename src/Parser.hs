module Parser where

import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

----- DO WE NEED THIS -----?
fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC  versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs
---------------------------------------------

data Commands = VarDecl String Expr
              | FunDecl String [(ArgType,String)] Expr
              | Fork Expr
              | Join
              | CommandPrint Expr
              -- | Bol String Expr
              -- | Int String Expr

data ArgType = Bol | Int

-- data Declaration =

data Functio = FunctionData String [Expr] Expr
    deriving Show

data Expr = Constant Integer
            | Identifier String
            | Mult Expr Expr
            | Add Expr Expr
            | Funct String [Expr]
            | If Condition Expr Expr
            | Paren Expr
            | Min Expr Expr
            | Print Expr
            deriving (Eq,Show)

data Condition = Lt Expr Expr
         | Eq Expr Expr
         | Gt Expr Expr
      deriving (Eq,Show)

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
                                      , "?"
                                      ]
            , Token.reservedOpNames = [ "+", "*", "-", "<", ">", "=="
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

comma :: Parser String
comma = Token.comma lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

parseCondition :: Parser Condition
parseCondition = try ((Eq <$> parseExpr) <*> (symbol "==" *> parseExpr))
              <|>try ((Lt <$> parseExpr) <*> (symbol "<" *> parseExpr))
              <|> ((Gt <$> parseExpr) <*> (symbol ">" *> parseExpr))
parseCondition_test1 = parse parseCondition "" "2 == fib(2)"
parseCondition_test2 = parse parseCondition "" "2 < fib(2)"
parseCondition_test3 = parse parseCondition "" "2 > fib(2)"

parseIf :: Parser Expr
parseIf = If <$> (reserved "?" *> parseCondition)
          <*> braces parseExpr
          <*> braces parseExpr
parseIf_tesst1 = parse parseIf "" "?(2==2){2+1}{2}"

-- parseExpr :: Parser Expr
-- parseExpr =

-- parseExpr :: Parser Expr
-- parseExpr = parseTerm `chainr1` addition
--             <|> parseDec
--             <|> parseIf


addition :: Parser (Expr -> Expr -> Expr)
addition = const Add <$> reservedOp "+"

subtraction :: Parser (Expr -> Expr -> Expr)
subtraction = const Min <$> reservedOp "-"

multiply :: Parser (Expr -> Expr -> Expr)
multiply = const Mult <$> reservedOp "*"


parseExpr:: Parser Expr
parseExpr = try( (parseTerm `chainr1` addition)
      <|> try ((parseTerm `chainl1` subtraction)
      <|> (parseTerm)))
parseExpr_test1 = parse parseExpr "" "3+3*2"
parseExpr_test2 = parse parseExpr "" "fib(2) + 32"
parseExpr_test3 = parse parseExpr "" "2 - 32"


-- Term parser
parseTerm :: Parser Expr
parseTerm = try (parseFactor `chainr1` multiply)
    <|> (parseFactor)

-- pareseFunctionDeclaration =
--   (Funct <$> identifier<*>(parens$ sep1 expr (char ',') ))

parameters :: Parser [Expr]
parameters = commaSep parseExpr

parseFactor :: Parser Expr
parseFactor = (Constant <$> integer)
      <|> parseIf
      <|> (Funct <$> identifier<*> parens parameters) --sepBy parseExpr comma )) -- Should be cool to use: commaSep
      <|> (Paren <$> parens parseExpr)
      <|> (Identifier <$> identifier)


parseDeclaration = ()
-- parseArgType = try (Bol<$>reserved "bool")
--             <|> try (Int<$>reserved "int")
-- parseVarDecl = try (VarDecl <$> parseArgType *>reserved "bool")

--
-- -- Condition parser
-- condition :: Parser Condition
-- condition = (Lt <$>expr<*>((symbol "<")*>expr) )
--           <|> (Eq <$>expr<*>((symbol "==")*>expr))
--           <|> (Gt <$>expr<*>((symbol ">")*>expr))
-- test_cond = runParser condition (Stream " 2 > 2 ")
--
-- -- Factor parser
-- factor :: Parser Expr
-- factor = (Constant <$> integer)
--       <|> (If <$>((symbol "if")*>parens condition)
--               <*>((symbol "then")*>curly expr)
--               <*>((symbol "else")*>curly expr))
--       <|> (Funct <$> identifier<*>(parens$ sep1 expr (char ',') ))
--       <|> (Paren <$> parens expr)
--       <|> (Identifier <$> identifier)
--
-- -- Decl parser (we name every function a declaration)
-- decl :: Parser Functio
-- decl = (FunctionData <$> identifier <*>
--                      (whitespace$ many$expr)      <*>
--                      ((symbol ":=")   *>
--                      (whitespace$ expr))          <*
--                      (whitespace$ (char ';'))
--        )
--
--
--







































--eof
