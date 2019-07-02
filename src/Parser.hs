module Parser where

import Data.Either
import Structure
import BasicParsers

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


--our language AST is made out of commands which in term have expressions in them
-- this file contains the parsers for commands and expression


-- commentline :: Parser String
-- commentline = Token.commentLine lexer

-- commentline :: Parser String
-- commentline = Token.commentLine lexer

------------------------Parsing Expression --------------------------

--parses Condition, it us used on parsing IfCom, IfExpr and While
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



--parses all expressions
parseExpr:: Parser Expr
parseExpr =
      try (Add <$> parseTerm<*>(reservedOp "+"*>parseExpr))
      <|> try ( Min <$> parseTerm<*>(reservedOp "-"*>parseExpr))
      <|> try (parseTerm)
      <|> try ((reserved "ya"  >> return (BoolConst True ))) -- parse Constant true
      <|> try ((reserved "nu"  >> return (BoolConst False ))) -- parse Constant false
parseExpr_test1 = parse parseExpr "" "3+3*2"
parseExpr_test2 = parse parseExpr "" "fib(2) + 32"
parseExpr_test3 = parse parseExpr "" "2 - 32 +2"
parseExpr_testnu = parse parseExpr "" "nu"
parseExpr_testya = parse parseExpr "" "ya"


-- Term parser
parseTerm :: Parser Expr
parseTerm =
    try (Mult <$> parseFactor<*>(reservedOp "*" *>parseTerm))
    <|> try (parseFactor)
parseTerm_test1 = parse parseTerm "" "2*4"
parseTerm_test2 = parse parseTerm "" "2*4*4"


--parses a list of expression
--it is used only in -> parseFactor while parsing a exprfunction call
arguments :: Parser [Expr]
arguments = commaSep parseExpr
arguments_test1 = parse arguments "" "2,x"

--parses only int and bool argument types because this methods is used in parsing varibales and arguments
--which cannot be void
parseType :: Parser Type
parseType = try (reserved "bool"  >> return SimplyBol )
          <|> try (reserved "int"  >> return SimplyInt )
parseType_testbool = parse parseType "" "bool"
parseType_testint = parse parseType "" "int"


parseIntType = try (reserved "int"  >> return SimplyInt )
parseBoolType = try (reserved "bool"  >> return SimplyBol )

--parses all types as this methods is used for parsing function declarations
-- it is able to parse void too
parseVoidType :: Parser Type
parseVoidType = try (reserved "void"  >> return SimplyNull )
          <|> try (reserved "bool"  >> return SimplyBol )
          <|> try (reserved "int"  >> return SimplyInt )
parseVoidType_test = parse parseVoidType "" "void"


parseIfExpr :: Parser Expr
parseIfExpr = IfExpr <$> parseType <*>(reserved "?" *> parens parseCondition)
          <*> braces parseExpr
          <*> braces parseExpr
parseIfexpr_tesst1 = parse parseIfExpr "" "int ?(2==2){2+1}{2}"
parseIfexpr_tesst2 = parse parseIfExpr "" "bool ?(2==2){nu}{ya}"

parseFactor :: Parser Expr
parseFactor = try (Constant <$> integer)
      <|> try parseIfExpr
      <|> try (Funct <$> identifier<*> parens arguments)
      <|> try (Paren <$> parens parseExpr)
      <|> try (Identifier <$> identifier)

parsefactor_testCosnt = parse parseFactor "" "2"
parsefactor_testIdent = parse parseFactor "" "x"
parsefactor_testFunct = parse parseFactor "" "fib(2,3,ya)"
parsefactor_testIfexpr = parse parseFactor "" "int ?(2<x){2+x}{2+3}"






----------------------------------------------------------------------------------
-----------------------Parse Commands----------------------------------------------

--this methods pares a single command
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


--parses a block, used in all the commands which have blocks , fundecl, while etc
--a program is a block, so when an entire program has to be parsed, parseBlock would be called
parseBlock :: Parser Bloc
parseBlock = Block <$> (reserved "{" *>  addEnd)
parseBlock_test1 = parse parseBlock "" "{ nop;nop;}"
parseBlock_test2 = fromRight (Block []) (parse parseBlock "" "{ int x = 2;nop;}")


-- parses a list of cammand then adds an End comamnd at the end, to delimit the ending of a block
-- used in -> parseBlock
-- it expect ";" after each command, so function declarations, ifcommands and while statements
--        _have_ to have a ";" after they end their blocks with "}
addEnd :: Parser [Commands]
addEnd = (++) <$> parseArrayCommands <*> ((:)<$>(reserved "}" >> return End) <*> pure [])
addEnd_test1 = parse addEnd "" "nop;}"

--methods that parses an array of commands, used in -> addEnd
parseArrayCommands :: Parser [Commands]
parseArrayCommands= many (parseCommand<*semi)
parseArrayCommands_test1 = parse parseArrayCommands "" "nop;nop;"

--parses while
parseWhile::Parser Commands
parseWhile = While <$>(reserved "while" *>parens parseCondition) <*> parseBlock
parseWhile_test1 = parse parseWhile "" "while ( x < 2 ) { x++;};"

--parses function call
parseFuncCall::Parser Commands
parseFuncCall = FunCall <$> identifier<*> parens arguments
parseFuncCall_test1 = parse parseFuncCall "" "demote(x,2);"

--parses a type,which can only be int and bool (for this method), and an identifier e
--this methods is used in ->parseParam,
parseArgType:: Parser ArgType
parseArgType = try (Arg <$>parseType<*>identifier)
parseArgType_testBol = parse parseArgType "" "bool x"
parseArgType_testInt = parse parseArgType "" "int x"

parseIntArgType = try (Arg <$> parseIntType <*> identifier)
parseBoolArgType = try (Arg <$> parseBoolType <*> identifier)

--parses all 3 types and then an identifier
--methods used in -> parseFunDecl, VarDecl and GlobalVarDecl
parseArgTypeVoid:: Parser ArgType
parseArgTypeVoid = Arg <$>parseVoidType<*>identifier
parseArgTypeVoid_testvoid = parse parseArgTypeVoid "" "void fib"
parseArgTypeVoid_testint = parse parseArgTypeVoid "" "int fib"
parseArgTypeVoid_testbool = parse parseArgTypeVoid "" "bool fib"

--parses variables declaration
parseVarDecl:: Parser Commands
parseVarDecl = try (VarDecl <$> parseArgType <*>(reservedOp "=" *> parseExpr))
           <|> try (VarDecl <$> parseBoolArgType <*>(return (BoolConst False)))
           <|> try (VarDecl <$> parseIntArgType <*>(return (Constant 0)))
parseVarDecl_testint1 = parse parseVarDecl "" "int x ;"
parseVarDecl_testint2 = parse parseVarDecl "" "int x = fib(2);"
parseVarDecl_testbool1 = parse parseVarDecl "" "bool x ;"
parseVarDecl_testbool2 = parse parseVarDecl "" "bool x = ya;"

--parses global variables delcation
parseGlobalVarDecl:: Parser Commands
parseGlobalVarDecl =   try (GlobalVarDecl <$>(reserved "global" *> parseArgType) <*>(reservedOp "=" *> parseExpr))
                  <|>  try (GlobalVarDecl <$>(reserved "global" *> parseIntArgType) <*>(return (Constant 0)))
                  <|>  try (GlobalVarDecl <$>(reserved "global" *>parseBoolArgType) <*>(return (BoolConst False)))
parseGlobalVarDecl_testbool1 = parse parseGlobalVarDecl "" "global bool x ;"
parseGlobalVarDecl_testbool2 = parse parseGlobalVarDecl "" "global bool x = ya;"
parseGlobalVarDecl_testint1 = parse parseGlobalVarDecl "" "global int x ;"
parseGlobalVarDecl_testint2 = parse parseGlobalVarDecl "" "global int x = 1;"

--parses assignment
parseAss:: Parser Commands
parseAss = try (Ass<$>identifier<*>(reservedOp "=" *> parseExpr))
parseAss_test1 = parse parseAss "" "x = 2;"
parseAss_test2 = parse parseAss "" "x = fib(2);"

--parses If Command, this type of if does not have a return
parseIfCom :: Parser Commands
parseIfCom = IfCom <$> (reserved "if" *> parens parseCondition)
          <*>  parseBlock
          <*>  parseBlock
parseIfCom_tesst1 = parse parseIfCom "" "if(2==2){x=2;}{nop;}"
parseIfCom_tesst2 = parse parseIfCom ""  "if (from >= amount) { from -= amount; to += amount;} {};"
parseIfCom_tesst3 = parse parseIfExpr "" "?(2==2){x =2}{x =4}"

--parses Nop, which is no operation
parseNop:: Parser Commands
parseNop = (reserved "nop"  >> return Nop )
parsenop_test1 = parse parseNop "" "nop"

--parses minus command
parseMinCom :: Parser Commands
parseMinCom = (MinCom <$> identifier <*>(reservedOp "-=" *>parseExpr))
parseMinCom_test1 = parse parseMinCom "" "x -= 2+3;"

--parses add command
parseAddCom :: Parser Commands
parseAddCom = (AddCom <$> identifier <*>(reservedOp "+=" *>parseExpr))
parseAddCom_test1 = parse parseAddCom "" "x += 2+3;"

--parses increment command
parseIncr :: Parser Commands
parseIncr = (Incr <$> identifier <*reservedOp "++")
parseIncr_test1 = parse parseIncr "" "x ++;"

--parses decrese command
parseDecr :: Parser Commands
parseDecr = (Decr <$> identifier <*reservedOp "--")
parseDecr_test1 = parse parseDecr "" "x --;"

--parses print command
parsePrint :: Parser Commands
parsePrint = (Print <$>(reserved "print"*>parseExpr))
parsePrint_test1 = parse parsePrint "" "print 2;"
parsePrint_test2 = parse parsePrint "" "print int ?(2<x){2+x}{2+3};"

--parses function declaration
parseFunDecl :: Parser Commands
parseFunDecl = try (FunDecl <$> (reserved "func" *> parseArgTypeVoid)
                    <*> parens params <*>parseBlock)
parseFunDecl_test1 = parse parseFunDecl "" "func int theStuff(int a, bool b){ int x = a;}"
parseFunDecl_test2 = parse parseFunDecl "" "func void theStuff(int a, bool b){ int x = a;}"

--parses one parameter of a function used in -> params
parseParam:: Parser Param
parseParam = try (ByRef <$> (reserved "&" *>parseArgType))
            <|> (ByVal <$> parseArgType)
parseParam__test1 = parse parseParam "" "& int x"

--parses a list of arguments, used in -> parseFunDecl
params :: Parser [Param]
params = commaSep (parseParam)
params_test1 = parse params "" "& bool h, \n int x, \n bool x"


bigtest = parse parseBlock ""
          "{ int jesse = 1000; global int robert = 1000; while(robert == 0){ jesse++;}; int marieke = 5000; func int transfer(& int from,& int to, int amount) { jesse++; if (from >= amount) { from -= amount; to += amount;} {};}; func void helicopterMoney(int to, int amount) { to += amount;  };  fork { helicopterMoney(jesse, 9000);};  fork { helicopterMoney(robert, 9000);}; join;  print  jesse;  };"


fromRight :: b -> Either a b -> b
fromRight b (Left _) = b
fromRight _ (Right b) = b






































--eof
