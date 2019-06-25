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

data Commands = VarDecl ArgType Expr
              | FunDecl String [ArgType] Bloc
              | Fork Commands
              | Join
              | Print Expr
              | Ass String Expr
              | IfCom Condition Bloc Bloc
              | Incr String
              | Decr String
              | AddCom String Expr
              | MinCom String Expr
              -- | Nop
              deriving Show

data ArgType = Bol String | Int String
      deriving Show

data Bloc = Block [Commands]
      deriving Show
-- data Declaration =

data Functio = FunctionData String [Expr] Expr
    deriving Show

data Expr = Constant Integer
            | Identifier String
            | Mult Expr Expr
            | Add Expr Expr
            | Funct String [Expr]
            | IfExpr Condition Expr Expr
            | Paren Expr
            | Min Expr Expr
            deriving (Eq,Show)

data Condition = Lt Expr Expr
         | Eq Expr Expr
         | Gt Expr Expr
         | Lq Expr Expr
         | Gq Expr Expr
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
                                      , "if"
                                      , "nop"
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

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semisep :: Parser a -> Parser [a]
semisep = Token.semiSep lexer

parseCondition :: Parser Condition
parseCondition = try ((Eq <$> parseExpr) <*> (symbol "==" *> parseExpr))
              <|>try ((Lt <$> parseExpr) <*> (symbol "<" *> parseExpr))
              <|> try ((Gt <$> parseExpr) <*> (symbol ">" *> parseExpr))
              <|> try ((Lq <$> parseExpr) <*> (symbol "<=" *> parseExpr))
              <|> ((Gq <$> parseExpr) <*> (symbol ">=" *> parseExpr))
parseCondition_test1 = parse parseCondition "" "2 == fib(2)"
parseCondition_test2 = parse parseCondition "" "2 < fib(2)"
parseCondition_test3 = parse parseCondition "" "2 > fib(2,2)"
parseCondition_test4 = parse parseCondition "" "2 <= fib(2,2)"
parseCondition_test5 = parse parseCondition "" "2 >= fib(2,2)"


addition :: Parser (Expr -> Expr -> Expr)
addition = const Add <$> reservedOp "+"

subtraction :: Parser (Expr -> Expr -> Expr)
subtraction = const Min <$> reservedOp "-"

multiply :: Parser (Expr -> Expr -> Expr)
multiply = const Mult <$> reservedOp "*"

semi :: Parser String
semi = Token.semi lexer


parseExpr:: Parser Expr
parseExpr =
      try (Add <$> parseTerm<*>(reservedOp "+"*>parseExpr))
      <|> try ( Min <$> parseTerm<*>(reservedOp "-"*>parseExpr))
      --  (parseTerm `chainl1` addition)
      -- <|>  (parseTerm `chainl1` subtraction)
      <|> try (parseTerm)

      -- parseExpr = try (Add <$> parseTerm<*>reservedOp "+"<*>parseTerm)
      --       <|> try ((parseTerm `chainl1` subtraction)
      --       <|> (parseTerm))
parseExpr_test1 = parse parseExpr "" "3+3*2"
parseExpr_test2 = parse parseExpr "" "fib(2) + 32"
parseExpr_test3 = parse parseExpr "" "2 - 32 +2"


-- Term parser
parseTerm :: Parser Expr
parseTerm =
    try (Mult <$> parseFactor<*>(reservedOp "*" *>parseTerm))
    -- try (parseFactor `chainl1` multiply)
    <|> try (parseFactor)
parseTerm_test1 = parse parseTerm "" "2*4"
parseTerm_test2 = parse parseTerm "" "2*4*4"

-- pareseFunctionDeclaration =
--   (Funct <$> identifier<*>(parens$ sep1 expr (char ',') ))

parameters :: Parser [Expr]
parameters = commaSep parseExpr
parameters_test1 = parse parameters "" "2,x"

parseIfExpr :: Parser Expr
parseIfExpr = IfExpr <$> (reserved "?" *> parens parseCondition)
          <*> braces parseExpr
          <*> braces parseExpr
parseIfexpr_tesst1 = parse parseIfExpr "" "?(2==2){2+1}{2}"
parseIfexpr_tesst2 = parse parseIfExpr "" "?(2==2){2}{3}"

parseFactor :: Parser Expr
parseFactor = try (Constant <$> integer)
      <|> try parseIfExpr
      <|> try (Funct <$> identifier<*> parens parameters) --sepBy parseExpr comma )) -- Should be cool to use: commaSep
      <|> try (Paren <$> parens parseExpr)
      <|> try (Identifier <$> identifier)

parsefactor_testCosnt = parse parseFactor "" "2"
parsefactor_testIdent = parse parseFactor "" "x"
parsefactor_testIfexpr = parse parseFactor "" "?(2<x){2+x}{2+3}"

-----------------------Parse Commands-----------------------------------
parseArgType:: Parser ArgType
parseArgType = try (Bol<$>(reserved "bool"*>identifier))
            <|> try (Int<$>(reserved "int"*>identifier))
parseArgType_testBol = parse parseArgType "" "bool x"
parseArgType_testInt = parse parseArgType "" "int x"

parseVarDecl:: Parser Commands
parseVarDecl = VarDecl <$> parseArgType <*>(reservedOp "=" *> parseExpr)<*semi
parseVarDecl_test1 = parse parseVarDecl "" "int x = 2;"
parseVarDecl_test2 = parse parseVarDecl "" "int x = fib(2);"

parseAss:: Parser Commands
parseAss = try (Ass<$>identifier<*>(reservedOp "=" *> parseExpr)<*semi)
parseAss_test1 = parse parseAss "" "x = 2;"
parseAss_test2 = parse parseAss "" "x = fib(2);"

parseIfCom :: Parser Commands
parseIfCom = IfCom <$> (reserved "if" *> parens parseCondition)
          <*> braces  parseBlock
          <*> braces parseBlock
parseIfCom_tesst1 = parse parseIfCom "" "if(2==2){x=2;}{x=3;}"
-- parseIfCom_tesst2 = parse parseIfExpr "" "?(2==2){x =2}{x =4}"

-- parseNop:: Parser Commands
-- parseNop = Nop <$> reserved "nop"


parseArrayCommands :: Parser [Commands]
parseArrayCommands = semisep parseCommand
parseArrayCommands_test1 = parse parseArrayCommands "" "x=2;y=3;3"

parseBlock :: Parser Bloc
parseBlock = Block <$> semisep parseCommand
parseBlock_test1 = parse parseBlock "" "x=2;y=3;"

parseMinCom :: Parser Commands
parseMinCom = (MinCom <$> identifier <*>(reservedOp "-=" *>parseExpr)<*semi)
parseMinCom_test1 = parse parseMinCom "" "x -= 2+3;"

parseAddCom :: Parser Commands
parseAddCom = (AddCom <$> identifier <*>(reservedOp "+=" *>parseExpr)<*semi)
parseAddCom_test1 = parse parseAddCom "" "x += 2+3;"

parseIncr :: Parser Commands
parseIncr = (Incr <$> identifier <*(reservedOp "++")<*semi)
parseIncr_test1 = parse parseIncr "" "x ++;"

parseDecr :: Parser Commands
parseDecr = (Decr <$> identifier <*(reservedOp "--")<*semi)
parseDecr_test1 = parse parseDecr "" "x --;"

parsePrint :: Parser Commands
parsePrint = (Print <$>(reserved "print"*>parseExpr)<*semi)
parsePrint_test1 = parse parsePrint "" "print 2;"
parsePrint_test2 = parse parsePrint "" "print ?(2<x){2+x}{2+3};"

parseCommand :: Parser Commands
parseCommand = try parseVarDecl
              <|> try parseAss
              <|> try parseIfCom


















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
