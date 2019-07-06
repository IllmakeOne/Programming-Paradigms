module Parser where

import Data.Either
import Structure
import BasicParsers
import ParseExpr

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token








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
           <|> try (Fork<$>(reserved"fork" *> parseExprFunCall))
           <|> try (Return<$>(reserved "return" *> parseExpr))
           <|> try parseNop
           <|> try parsePrint
           <|> try parseIncr
           <|> try parseDecr
           <|> try parseMinCom
           <|> try parseAddCom
           <|> try parseFuncCall
           <|> try parseWhile
parseCommand_testJoin = parse parseCommand "" "join ;" == Right Join
parseCommand_testFork = parse parseCommand "" "fork fib(2,3);"
parseCommand_testReturn = parse parseCommand "" "return int ?(x<2) {2}{3};" == Right (Return (IfExpr SimplyInt (Lt (Identifier "x") (Constant 2)) (Constant 2) (Constant 3)))


--parses a block, used in all the commands which have blocks , fundecl, while etc
--a program is a block, so when an entire program has to be parsed, parseBlock would be called
parseBlock :: Parser Bloc
parseBlock = Block <$> (reserved "{" *>  addEnd)
parseBlock_test1 = parse parseBlock "" "{ nop;nop;}" == Right (Block [Nop,Nop,End])
parseBlock_test2 = fromRight (Block []) (parse parseBlock "" "{ int x = 2;nop;}") == Block [VarDecl (Arg SimplyInt "x") (Constant 2),Nop,End]


-- parses a list of cammand then adds an End comamnd at the end, to delimit the ending of a block
-- used in -> parseBlock
-- it expect ";" after each command, so function declarations, ifcommands and while statements
--        _have_ to have a ";" after they end their blocks with "}
addEnd :: Parser [Commands]
addEnd = (++) <$> parseArrayCommands <*> ((:)<$>(reserved "}" >> return End) <*> pure [])
addEnd_test1 = parse addEnd "" "nop;}" == Right [Nop,End]

--methods that parses an array of commands, used in -> addEnd
parseArrayCommands :: Parser [Commands]
parseArrayCommands= many (parseCommand<*semi)
parseArrayCommands_test1 = parse parseArrayCommands "" "nop;nop;" == Right [Nop,Nop]

--parses while
parseWhile::Parser Commands
parseWhile = While <$>(reserved "while" *>parens parseCondition) <*> parseBlock
parseWhile_test1 = parse parseWhile "" "while ( x < 2 ) { x++;};" == Right (While (Lt (Identifier "x") (Constant 2)) (Block [Incr "x",End]))
parseWhile_test2 = parse parseWhile "" "while (ya >= 2) { int x;};" ==Right (While (Gq (BoolConst True) (Constant 2)) (Block [VarDecl (Arg SimplyInt "x") (Constant 0),End]))

--parses function call
parseFuncCall::Parser Commands
parseFuncCall = FunCall <$> identifier<*> parens arguments
parseFuncCall_test1 = parse parseFuncCall "" "demote(x,2);" == Right (FunCall "demote" [Identifier "x",Constant 2])

--parses a type,which can only be int and bool (for this method), and an identifier e
--this methods is used in ->parseParam,
parseArgType:: Parser ArgType
parseArgType = try (Arg <$>parseType<*>identifier)
parseArgType_testBol = parse parseArgType "" "bool x" == Right (Arg SimplyBol "x")
parseArgType_testInt = parse parseArgType "" "int x" == Right (Arg SimplyInt "x")

parseIntArgType = try (Arg <$> parseIntType <*> identifier)
parseBoolArgType = try (Arg <$> parseBoolType <*> identifier)

--parses all 3 types and then an identifier
--methods used in -> parseFunDecl, VarDecl and GlobalVarDecl
parseArgTypeVoid:: Parser ArgType
parseArgTypeVoid = Arg <$>parseVoidType<*>identifier
parseArgTypeVoid_testvoid = parse parseArgTypeVoid "" "void fib" == Right (Arg SimplyNull "fib")
parseArgTypeVoid_testint = parse parseArgTypeVoid "" "int fib" == Right (Arg SimplyInt "fib")
parseArgTypeVoid_testbool = parse parseArgTypeVoid "" "bool fib" == Right (Arg SimplyBol "fib")

--parses variables declaration
parseVarDecl:: Parser Commands
parseVarDecl = try (VarDecl <$> parseArgType <*>(reservedOp "=" *> parseExpr))
           <|> try (VarDecl <$> parseBoolArgType <*>(return (BoolConst False)))
           <|> try (VarDecl <$> parseIntArgType <*>(return (Constant 0)))
parseVarDecl_testint1 = parse parseVarDecl "" "int x ;" == Right (VarDecl (Arg SimplyInt "x") (Constant 0))
parseVarDecl_testint2 = parse parseVarDecl "" "int x = fib(2);" == Right (VarDecl (Arg SimplyInt "x") (Funct "fib" [Constant 2]))
parseVarDecl_testbool1 = parse parseVarDecl "" "bool x ;" == Right (VarDecl (Arg SimplyBol "x") (BoolConst False))
parseVarDecl_testbool2 = parse parseVarDecl "" "bool x = ya;" == Right (VarDecl (Arg SimplyBol "x") (BoolConst True))

--parses global variables delcation
parseGlobalVarDecl:: Parser Commands
parseGlobalVarDecl =   try (GlobalVarDecl <$>(reserved "global" *> parseArgType) <*>(reservedOp "=" *> parseExpr))
                  <|>  try (GlobalVarDecl <$>(reserved "global" *> parseIntArgType) <*>(return (Constant 0)))
                  <|>  try (GlobalVarDecl <$>(reserved "global" *>parseBoolArgType) <*>(return (BoolConst False)))
parseGlobalVarDecl_testbool1 = parse parseGlobalVarDecl "" "global bool x ;" == Right (GlobalVarDecl (Arg SimplyBol "x") (BoolConst False))
parseGlobalVarDecl_testbool2 = parse parseGlobalVarDecl "" "global bool x = ya;" == Right (GlobalVarDecl (Arg SimplyBol "x") (BoolConst True))
parseGlobalVarDecl_testint1 = parse parseGlobalVarDecl "" "global int x ;" == Right (GlobalVarDecl (Arg SimplyInt "x") (Constant 0))
parseGlobalVarDecl_testint2 = parse parseGlobalVarDecl "" "global int x = 1;" == Right (GlobalVarDecl (Arg SimplyInt "x") (Constant 1))

--parses assignment
parseAss:: Parser Commands
parseAss = try (Ass<$>identifier<*>(reservedOp "=" *> parseExpr))
parseAss_test1 = parse parseAss "" "x = 2;" == Right (Ass "x" (Constant 2))
parseAss_test2 = parse parseAss "" "x = fib(2);" == Right (Ass "x" (Funct "fib" [Constant 2]))

--parses If Command, this type of if does not have a return
parseIfCom :: Parser Commands
parseIfCom = IfCom <$> (reserved "if" *> parens parseCondition)
          <*>  parseBlock
          <*>  parseBlock
parseIfCom_tesst1 = parse parseIfCom "" "if (x >=2) { print x;}{ print 2;}  "
          == Right (IfCom (Gq (Identifier "x") (Constant 2)) (Block [Print (Identifier "x"),End]) (Block [Print (Constant 2),End]))
parseIfCom_tesst2 = parse parseIfCom ""  "if (from >= amount) { from -= amount; to += amount;} {};"
          == Right (IfCom (Gq (Identifier "from") (Identifier "amount")) (Block [MinCom "from" (Identifier "amount"),AddCom "to" (Identifier "amount"),End]) (Block [End]))


--parses Nop, which is no operation
parseNop:: Parser Commands
parseNop = (reserved "nop"  >> return Nop )
parsenop_test1 = parse parseNop "" "nop" == Right Nop

--parses minus command
parseMinCom :: Parser Commands
parseMinCom = (MinCom <$> identifier <*>(reservedOp "-=" *>parseExpr))
parseMinCom_test1 = parse parseMinCom "" "x -= 2+3;" == Right (MinCom "x" (Add (Constant 2) (Constant 3)))

--parses add command
parseAddCom :: Parser Commands
parseAddCom = (AddCom <$> identifier <*>(reservedOp "+=" *>parseExpr))
parseAddCom_test1 = parse parseAddCom "" "x += 2+3;" == Right (AddCom "x" (Add (Constant 2) (Constant 3)))

--parses increment command
parseIncr :: Parser Commands
parseIncr = (Incr <$> identifier <*reservedOp "++")
parseIncr_test1 = parse parseIncr "" "x ++;" == Right (Incr "x")

--parses decrese command
parseDecr :: Parser Commands
parseDecr = (Decr <$> identifier <*reservedOp "--")
parseDecr_test1 = parse parseDecr "" "x --;" == Right (Decr "x")

--parses print command
parsePrint :: Parser Commands
parsePrint = (Print <$>(reserved "print"*>parseExpr))
parsePrint_test1 = parse parsePrint "" "print 2;" == Right (Print (Constant 2))
parsePrint_test2 = parse parsePrint "" "print int ?(2<x){2+x}{2+3};"
        == Right (Print (IfExpr SimplyInt (Lt (Constant 2) (Identifier "x")) (Add (Constant 2) (Identifier "x")) (Add (Constant 2) (Constant 3))))

--parses function declaration
parseFunDecl :: Parser Commands
parseFunDecl = try (FunDecl <$> (reserved "func" *> parseArgTypeVoid)
                    <*> parens params <*>parseBlock)
parseFunDecl_test1 = parse parseFunDecl "" "func int theStuff(int a, & bool b){ int x = a;}"
        == Right (FunDecl (Arg SimplyInt "theStuff") [ByVal (Arg SimplyInt "a"),ByRef (Arg SimplyBol "b")] (Block [VarDecl (Arg SimplyInt "x") (Identifier "a"),End]))
parseFunDecl_test2 = parse parseFunDecl "" "func void theStuff(int a, bool b){ int x = a;}"
        == Right (FunDecl (Arg SimplyNull "theStuff") [ByVal (Arg SimplyInt "a"),ByVal (Arg SimplyBol "b")] (Block [VarDecl (Arg SimplyInt "x") (Identifier "a"),End]))

--parses one parameter of a function used in -> params
parseParam:: Parser Param
parseParam = try (ByRef <$> (reserved "&" *>parseArgType))
            <|> (ByVal <$> parseArgType)
parseParam__test1 = parse parseParam "" "& int x" == Right (ByRef (Arg SimplyInt "x"))
parseParam__test2 = parse parseParam "" "int x" == Right (ByVal (Arg SimplyInt "x"))
--parses a list of arguments, used in -> parseFunDecl
params :: Parser [Param]
params = commaSep (parseParam)
params_test1 = parse params "" "& bool h, \n int x, \n bool x"
        == Right [ByRef (Arg SimplyBol "h"),ByVal (Arg SimplyInt "x"),ByVal (Arg SimplyBol "x")]



































--eof
