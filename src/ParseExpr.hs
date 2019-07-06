module ParseExpr where

import Data.Either
import Structure
import BasicParsers

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



------------------------Parsing Expression --------------------------

--parses Condition, it us used on parsing IfCom, IfExpr and While
parseCondition :: Parser Condition
parseCondition = try ((Eq <$> parseExpr) <*> (symbol "==" *> parseExpr))
              <|>try ((Lt <$> parseExpr) <*> (symbol "<" *> parseExpr))
              <|> try ((Gt <$> parseExpr) <*> (symbol ">" *> parseExpr))
              <|> try ((Lq <$> parseExpr) <*> (symbol "<=" *> parseExpr))
              <|> ((Gq <$> parseExpr) <*> (symbol ">=" *> parseExpr))
parseCondition_test1 = parse parseCondition "" "2 == fib(2)" == Right (Eq (Constant 2) (Funct "fib" [Constant 2]))
parseCondition_test2 = parse parseCondition "" "2 < fib(2)" == Right (Lt (Constant 2) (Funct "fib" [Constant 2]))
parseCondition_test3 = parse parseCondition "" "2 > fib(2,2)" == Right (Gt (Constant 2) (Funct "fib" [Constant 2,Constant 2]))
parseCondition_test4 = parse parseCondition "" "2 <= fib(2,2)" == Right (Lq (Constant 2) (Funct "fib" [Constant 2,Constant 2]))
parseCondition_test5 = parse parseCondition "" "2 >= fib(2,2)" == Right (Gq (Constant 2) (Funct "fib" [Constant 2,Constant 2]))


addition :: Parser (Expr -> Expr -> Expr)
addition = const Add <$> symbol "+"

substraction :: Parser (Expr -> Expr -> Expr)
substraction = const Min <$> symbol "-"

mult :: Parser (Expr -> Expr -> Expr)
mult = const Mult <$> symbol "*"



--parses all expressions
parseExpr:: Parser Expr
parseExpr = try parseExprIntermed `chainr1` addition
            <|> try parseExprIntermed
parseExpr_test1 = parse parseExpr "" "3+3*2" == Right (Add (Constant 3) (Mult (Constant 3) (Constant 2)))
parseExpr_test2 = parse parseExpr "" "fib(2) + 32" == Right (Add (Funct "fib" [Constant 2]) (Constant 32))
parseExpr_testnu = parse parseExpr "" "nu" == Right (BoolConst False)
parseExpr_testya = parse parseExpr "" "ya"== Right (BoolConst True)
parseExpr_test3 = parse parseExpr "" "(2-3+3)*3" == Right (Mult (Paren (Add (Min (Constant 2) (Constant 3)) (Constant 3))) (Constant 3))
parseExpr_test4 = parse parseExpr "" "2*3*3-3" == Right (Min (Mult (Constant 2) (Mult (Constant 3) (Constant 3))) (Constant 3))

parseExprIntermed :: Parser Expr
parseExprIntermed = parseTerm `chainr1` substraction
              <|> try parseTerm


-- Term parser
parseTerm :: Parser Expr
parseTerm =
    parseFactor `chainr1` mult
    <|> try (parseFactor)
parseTerm_test1 = parse parseTerm "" "2*4" == Right (Mult (Constant 2) (Constant 4))
parseTerm_test2 = parse parseTerm "" "2*4*4" == Right (Mult (Constant 2) (Mult (Constant 4) (Constant 4)))


-- Factor parser, lowerst ranked
parseFactor :: Parser Expr
parseFactor = try (Constant <$> integer)
      <|> try parseIfExpr
      <|> try parseExprFunCall
      <|> try (Paren <$> parens parseExpr)
      <|> try (Identifier <$> identifier)
      <|> try ((reserved "ya"  >> return (BoolConst True ))) -- parse Constant true
      <|> try ((reserved "nu"  >> return (BoolConst False ))) -- parse Constant falses
parsefactor_testCosnt = parse parseFactor "" "2" == Right (Constant 2)
parsefactor_testIdent = parse parseFactor "" "x" == Right (Identifier "x")
parsefactor_testFunct = parse parseFactor "" "fib(2,3,ya)" == Right (Funct "fib" [Constant 2,Constant 3,BoolConst True])
parsefactor_testIfexpr = parse parseFactor "" "int ?(2<x){2+x}{2+3}" == Right (IfExpr SimplyInt (Lt (Constant 2) (Identifier "x")) (Add (Constant 2) (Identifier "x")) (Add (Constant 2) (Constant 3)))


parseExprFunCall :: Parser Expr
parseExprFunCall = try (Funct <$> identifier<*> parens arguments)
--parses a list of expression
--it is used only in -> parseFactor while parsing a exprfunction call
arguments :: Parser [Expr]
arguments = commaSep parseExpr
arguments_test1 = parse arguments "" "2,x" == Right [Constant 2,Identifier "x"]

--parses only int and bool argument types because this methods is used in parsing varibales and arguments
--which cannot be void
parseType :: Parser Type
parseType = try (reserved "bool"  >> return SimplyBol )
          <|> try (reserved "int"  >> return SimplyInt )
parseType_testbool = parse parseType "" "bool" == Right SimplyBol
parseType_testint = parse parseType "" "int" == Right SimplyInt

--helper methods used in VarDecl
parseIntType = try (reserved "int"  >> return SimplyInt )
parseBoolType = try (reserved "bool"  >> return SimplyBol )

--parses all types as this methods is used for parsing function declarations
-- it is able to parse void too
parseVoidType :: Parser Type
parseVoidType = try (reserved "void"  >> return SimplyNull )
          <|> try (reserved "bool"  >> return SimplyBol )
          <|> try (reserved "int"  >> return SimplyInt )
parseVoidType_test = parse parseVoidType "" "void" == Right SimplyNull

-- parses an if expression which looks like : (int/bool) ? (condition){expr1}{expr2}
-- an if expression ahs to return a value
parseIfExpr :: Parser Expr
parseIfExpr = IfExpr <$> parseType <*>(reserved "?" *> parens parseCondition)
              <*> braces parseExpr <*> braces parseExpr
parseIfexpr_tesst1 = parse parseIfExpr "" "int ?(2==2){2+1}{2}" == Right (IfExpr SimplyInt (Eq (Constant 2) (Constant 2)) (Add (Constant 2) (Constant 1)) (Constant 2))
parseIfexpr_tesst2 = parse parseIfExpr "" "bool ?(2==2){nu}{ya}" == Right (IfExpr SimplyBol (Eq (Constant 2) (Constant 2)) (BoolConst False) (BoolConst True))


















--eof
