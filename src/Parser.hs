module Parser where

import Data.Either
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



data Commands = VarDecl ArgType Expr          -- DONE IMPLEMENTED IN GENERATOR
              | GlobalVarDecl ArgType Expr
              | FunDecl ArgType [Param] Bloc
              | FunCall String [Expr]
              | Fork Bloc
              | Join
              | Print Expr                -- DONE IMPLEMENTED IN GENERATOR
              | Ass String Expr           -- DONE IMPLEMENTED IN GENERATOR
              | IfCom Condition Bloc Bloc -- DONE IMPLEMENTED IN GENERATOR
              | While Condition Bloc      -- DONE IMPLEMENTED IN GENERATOR
              | Incr String         -- DONE IMPLEMENTED IN GENERATOR
              | Decr String         -- DONE IMPLEMENTED IN GENERATOR
              | AddCom String Expr  -- DONE IMPLEMENTED IN GENERATOR
              | MinCom String Expr  -- DONE IMPLEMENTED IN GENERATOR
              | Comment String      --TODO use endby
              | Nop                 -- DONE IMPLEMENTED IN GENERATOR
              | End                 -- DONE IMPLEMENTED IN GENERATOR
              | Return Expr
            deriving (Eq,Show)

data Param = ByVal ArgType | ByRef ArgType
      deriving (Eq,Show)

data ArgType = Arg Type String -- Bol String | Int String | Void String
      deriving (Eq,Show)

data Type = SimplyBol | SimplyInt | SimplyNull
      deriving (Eq,Show)


------------delete tthis

data Affir = Da | Nu
      deriving (Eq,Show)
data Random = Ugh Affir String
      deriving (Eq,Show)
parseAFir :: Parser Random
parseAFir = try (Ugh<$>(reserved "ya" >> return Da )<*>identifier)
      <|> try (Ugh<$>(reserved "nu" >> return Nu) <*>identifier)
parseAFir_test = parse parseAFir "" "ya asd"
------------------



data Bloc = Block [Commands]
      deriving (Eq,Show)
-- data Declaration =


data Expr = Constant Integer -- DONE IMPLEMENTED IN GENERATOR
            | BoolConst Bool -- DONE IMPLEMENTED
            | Identifier String -- DONE IMPLEMENTED IN GENERATOR
            | Mult Expr Expr -- DONE IMPLEMENTED
            | Add Expr Expr -- DONE IMPLEMENTED
            | Funct String [Expr]
            | IfExpr Type Condition Expr Expr -- inline if statement
            | Paren Expr    -- DONE IMPLEMENTED
            | Min Expr Expr -- DONE IMPLEMENTED
            deriving (Eq,Show)

data Condition = Lt Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Eq Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Gt Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Lq Expr Expr -- DONE IMPLEMENTED IN GENERATOR
         | Gq Expr Expr -- DONE IMPLEMENTED IN GENERATOR
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

parseType :: Parser Type
parseType = try (reserved "bool"  >> return SimplyBol )
          <|> try (reserved "int"  >> return SimplyInt )
parseType_testbool = parse parseType "" "bool"
parseType_testint = parse parseType "" "int"

parseVoidType :: Parser Type
parseVoidType = try (reserved "void"  >> return SimplyNull )
parseVoidType_test = parse parseVoidType "" "void"


parseIfExpr :: Parser Expr
parseIfExpr = IfExpr <$> parseType <*>(reserved "?" *> parens parseCondition)
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
           <|> try (Fork<$>(reserved"fork" *> parseBlock))
           <|> try (Return<$>(reserved "return" *> parseExpr))
           <|> try parseNop
           <|> try parsePrint
           <|> try parseIncr
           <|> try parseDecr
           <|> try parseMinCom
           <|> try parseAddCom
           <|> try parseFuncCall
           <|> try parseWhile
parseCommand_testJoin = parse parseCommand "" "join ;"
parseCommand_testFork = parse parseCommand "" "fork {int x = 2;};"
parseCommand_testReturn = parse parseCommand "" "return int ?(x<2) {2}{3};"




parseBlock :: Parser Bloc
-- parseBlock = Block <$> parseArrayCommands
-- parseBlock = Block <$> (reserved "{" *> (optional spaces)*> addEnd)
parseBlock = Block <$> (reserved "{" *>  addEnd)
parseBlock_test1 = parse parseBlock "" "{ nop;nop;}"
parseBlock_test2 = fromRight (Block []) (parse parseBlock "" "{ int x = 2;nop;}")

parseArrayCommands :: Parser [Commands]
-- parseArrayCommands= (++) <$> many (parseCommand<*semi) <*>parseEnd
parseArrayCommands= many (parseCommand<*semi)
parseArrayCommands_test1 = parse parseArrayCommands "" "nop;nop;"

addEnd :: Parser [Commands]
addEnd = (++) <$> parseArrayCommands <*> ((:)<$>(reserved "}" >> return End) <*> pure [])
addEnd_test1 = parse addEnd "" "nop;}"

parseWhile::Parser Commands
parseWhile = While <$>(reserved "while" *>parens parseCondition) <*> parseBlock
parseWhile_test1 = parse parseWhile "" "while ( x < 2 ) { x++;};"

parseFuncCall::Parser Commands
parseFuncCall = FunCall <$> identifier<*> parens parameters
parseFuncCall_test1 = parse parseFuncCall "" "demote(x,2);"

parseArgType:: Parser ArgType
parseArgType = try (Arg <$>parseType<*>identifier)
parseArgType_testBol = parse parseArgType "" "bool x"
parseArgType_testInt = parse parseArgType "" "int x"

parseArgTypeVoid:: Parser ArgType
parseArgTypeVoid = Arg <$>parseVoidType<*>identifier
parseArgTypeVoid_testvoid = parse parseArgTypeVoid "" "void fib"

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
          <*>  parseBlock
          <*>  parseBlock
parseIfCom_tesst1 = parse parseIfCom "" "if(2==2){x=2;}{nop;}"
parseIfCom_tesst2 = parse parseIfCom ""  "if (from >= amount) { from -= amount; to += amount;} {};"
parseIfCom_tesst3 = parse parseIfExpr "" "?(2==2){x =2}{x =4}"

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
parseFunDecl = try (FunDecl <$> (reserved "func" *> parseArgType)
                    <*> parens params <*>parseBlock)
              <|> FunDecl <$> (reserved "func" *> parseArgTypeVoid)
                                  <*> parens params <*>parseBlock
parseFunDecl_test1 = parse parseFunDecl "" "func int theStuff(int a, bool b){int x = a;}"

parseParam:: Parser Param
parseParam = try (ByRef <$> (reserved "&" *>parseArgType))
            <|> (ByVal <$> parseArgType)
parseParam__test1 = parse parseParam "" "& int x"

params :: Parser [Param]
params = commaSep (parseParam)
params_test1 = parse params "" "& bool h, \n int x, \n bool x"

-- theWholeShabang :: Parser Bloc
-- theWholeShabang =


bigtest = parse parseBlock ""
          "{ int jesse = 1000; global int robert = 1000; while(robert == 0){ jesse++;}; int marieke = 5000; func int transfer(& int from,& int to, int amount) { jesse++; if (from >= amount) { from -= amount; to += amount;} {};}; func void helicopterMoney(int to, int amount) { to += amount;  };  fork { helicopterMoney(jesse, 9000);};  fork { helicopterMoney(robert, 9000);}; join;  print  jesse;  };"






fromRight :: b -> Either a b -> b
fromRight b (Left _) = b
fromRight _ (Right b) = b






































--eof
