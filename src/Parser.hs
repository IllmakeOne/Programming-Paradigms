module Parser where

import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



data Commands = VarDecl ArgType Expr
              | GlobalVarDecl ArgType Expr
              | FunDecl ArgType [ArgType] Bloc
              | Fork Bloc
              | Join
              | Print Expr
              | Ass String Expr
              | IfCom Condition Bloc Bloc
              | Incr String
              | Decr String
              | AddCom String Expr
              | MinCom String Expr
              | Comment String      --TODO use endby
              | Nop
              | End
              deriving Show

data ArgType = Bol String | Int String
      deriving (Eq,Show)

data IfType = SimplyBol | SimplyInt
      deriving (Eq,Show)


data Bloc = Block [Commands]
      deriving Show
-- data Declaration =


data Expr = Constant Integer
            | BoolConst Bool
            | Identifier String
            | Mult Expr Expr
            | Add Expr Expr
            | Funct String [Expr]
            | IfExpr IfType Condition Expr Expr
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
                                      , "ya"
                                      , "nu"
                                      , "global"
                                      , "{", "}"
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

-- commentline :: Parser String
-- commentline = Token.commentLine lexer

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


--
-- commentline :: Parser String
-- commentline = Token.commentLine lexer


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




parseExpr:: Parser Expr
parseExpr =
      try (Add <$> parseTerm<*>(reservedOp "+"*>parseExpr))
      <|> try ( Min <$> parseTerm<*>(reservedOp "-"*>parseExpr))
      --  (parseTerm `chainl1` addition)
      -- <|>  (parseTerm `chainl1` subtraction)
      <|> try (parseTerm)
      <|> try ((reserved "ya"  >> return (BoolConst True )))
      <|> try ((reserved "nu"  >> return (BoolConst False )))

      -- parseExpr = try (Add <$> parseTerm<*>reservedOp "+"<*>parseTerm)
      --       <|> try ((parseTerm `chainl1` subtraction)
      --       <|> (parseTerm))
parseExpr_test1 = parse parseExpr "" "3+3*2"
parseExpr_test2 = parse parseExpr "" "fib(2) + 32"
parseExpr_test3 = parse parseExpr "" "2 - 32 +2"
parseExpr_testnu = parse parseExpr "" "nu"
parseExpr_testya = parse parseExpr "" "ya"


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

parseIfType :: Parser IfType
parseIfType = try (reserved "bool"  >> return SimplyBol )
          <|> try (reserved "int"  >> return SimplyInt )
parseIfType_testbool = parse parseIfType "" "bool"
parseIfType_testint = parse parseIfType "" "int"

parseIfExpr :: Parser Expr
parseIfExpr = IfExpr <$> parseIfType <*>(reserved "?" *> parens parseCondition)
          <*> braces parseExpr
          <*> braces parseExpr
parseIfexpr_tesst1 = parse parseIfExpr "" "int ?(2==2){2+1}{2}"
-- parseIfexpr_tesst2 = parse parseIfExpr "" "bool ?(2==2){nu}{ya}"

parseFactor :: Parser Expr
parseFactor = try (Constant <$> integer)
      <|> try parseIfExpr
      <|> try (Funct <$> identifier<*> parens parameters) --sepBy parseExpr comma )) -- Should be cool to use: commaSep
      <|> try (Paren <$> parens parseExpr)
      <|> try (Identifier <$> identifier)

parsefactor_testCosnt = parse parseFactor "" "2"
parsefactor_testIdent = parse parseFactor "" "x"
parsefactor_testIfexpr = parse parseFactor "" "?(2<x){2+x}{2+3}"






----------------------------------------------------------------------------------
-----------------------Parse Commands----------------------------------------------

parseCommand :: Parser Commands
parseCommand = try parseVarDecl
           <|> try parseGlobalVarDecl
           <|> try parseAss
           <|> try parseIfCom
           <|> try parseFunDecl
           <|> try (reserved "join"  >> return Join )
           <|> try (Fork<$>(reserved"fork " *> parseBlock))
           <|> try parseNop
           <|> try parsePrint
           <|> try parseIncr
           <|> try parseDecr
           <|> try parseMinCom
           <|> try parseAddCom

parseCommand_testJoin = parse parseCommand "" "join ;"
parseCommand_testFork = parse parseCommand "" "fork {int x = 2;};"




parseBlock :: Parser Bloc
-- parseBlock = Block <$> parseArrayCommands
-----------------------------TAAAAAAAAAAAAAAAAAAa-----------------------------
parseBlock = Block <$> (reserved "{" *> (optional spaces)*> addEnd)
parseBlock_test1 = parse parseBlock "" "{ nop;nop;}"
parseBlock_test2 = fromRight (Block []) (parse parseBlock "" "{ int x = 2;nop;}")


parseArrayCommands :: Parser [Commands]
-- parseArrayCommands= (++) <$> many (parseCommand<*semi) <*>parseEnd
parseArrayCommands= many (parseCommand<*semi)
parseArrayCommands_test1 = parse parseArrayCommands "" "nop;nop;"

addEnd :: Parser [Commands]
-- parseArrayCommands= (++) <$> many (parseCommand<*semi) <*>parseEnd
addEnd = (++) <$> parseArrayCommands <*> ((:)<$>(reserved "}" >> return End) <*> pure [])
addEnd_test1 = parse addEnd "" "nop;}"

parseArgType:: Parser ArgType
parseArgType = try (Bol<$>(reserved "bool"*>identifier))
            <|> try (Int<$>(reserved "int"*>identifier))
parseArgType_testBol = parse parseArgType "" "bool x"
parseArgType_testInt = parse parseArgType "" "int x"

parseVarDecl:: Parser Commands
parseVarDecl = VarDecl <$> parseArgType <*>(reservedOp "=" *> parseExpr)
parseVarDecl_test1 = parse parseVarDecl "" "int x = 2;"
parseVarDecl_test2 = parse parseVarDecl "" "int x = fib(2);"

parseGlobalVarDecl:: Parser Commands
parseGlobalVarDecl = GlobalVarDecl <$>(reserved "global" *> parseArgType) <*>(reservedOp "=" *> parseExpr)
parseGlobalVarDecl_test1 = parse parseGlobalVarDecl "" "global bool x = nu;"
parseGlobalVarDecl_test2 = parse parseGlobalVarDecl "" "global int x = fib(2);"

parseAss:: Parser Commands
parseAss = try (Ass<$>identifier<*>(reservedOp "=" *> parseExpr))
parseAss_test1 = parse parseAss "" "x = 2;"
parseAss_test2 = parse parseAss "" "x = fib(2);"

parseIfCom :: Parser Commands
parseIfCom = IfCom <$> (reserved "if" *> parens parseCondition)
          <*> braces  parseBlock
          <*> braces parseBlock
parseIfCom_tesst1 = parse parseIfCom "" "if(2==2){x=2;}{nop;}"
parseIfCom_tesst2 = parse parseIfExpr "" "?(2==2){x =2}{x =4}"

parseNop:: Parser Commands
parseNop = (reserved "nop"  >> return Nop )
parsenop_test1 = parse parseNop "" "nop"



parseMinCom :: Parser Commands
parseMinCom = (MinCom <$> identifier <*>(reservedOp "-=" *>parseExpr))
parseMinCom_test1 = parse parseMinCom "" "x -= 2+3;"

parseAddCom :: Parser Commands
parseAddCom = (AddCom <$> identifier <*>(reservedOp "+=" *>parseExpr))
parseAddCom_test1 = parse parseAddCom "" "x += 2+3;"

parseIncr :: Parser Commands
parseIncr = (Incr <$> identifier <*reservedOp "++")
parseIncr_test1 = parse parseIncr "" "x ++;"

parseDecr :: Parser Commands
parseDecr = (Decr <$> identifier <*reservedOp "--")
parseDecr_test1 = parse parseDecr "" "x --;"

-- parseComment :: Parser Commands
-- parseComment = (Comment <$> (commentLine <*semi))
-- parseComment_test1 = parse parseComment "" "x --;"

parsePrint :: Parser Commands
parsePrint = (Print <$>(reserved "print"*>parseExpr))
parsePrint_test1 = parse parsePrint "" "print 2;"
parsePrint_test2 = parse parsePrint "" "print ?(2<x){2+x}{2+3};"

parseFunDecl :: Parser Commands
parseFunDecl = FunDecl <$> (reserved "func" *> parseArgType)
                    <*> parens params <*>parseBlock
parseFunDecl_test1 = parse parseFunDecl "" "func int theStuff(int a, bool b){int x = a;}"

params :: Parser [ArgType]
params = commaSep parseArgType
params_test1 = parse params "" "bool h, \n int x, \n bool x"

-- theWholeShabang :: Parser Bloc
-- theWholeShabang =


bigtest = parse parseBlock "" "{ global int x = 2; bool y = 2; \n func int fib(int x) \n { x = 2; int y=3;}; \nprint x;}"





fromRight :: b -> Either a b -> b
fromRight b (Left _) = b
fromRight _ (Right b) = b






































--eof
