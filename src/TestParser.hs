module TestParser where

  data Commands = VarDecl String Expr
                | FunDecl String [(ArgType,String)] Expr
                | Fork Expr
                | Join

  data ArgType = Bol | Int

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
        
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


parseCondition :: Parser Condition
parseCondition = (Eq <$> parseExpr) <*> (symbol "==" *> parseExpr)
              <|>(Lt <$> parseExpr) <*> (symbol "<" *> parseExpr)
              <|>(Gt <$> parseExpr) <*> (symbol ">" *> parseExpr)
parseCondition_test1 = parse parseCondition "" "2 == fib(2)"
parseCondition_test2 = parse parseCondition "" "2 < fib(2)"
parseCondition_test3 = parse parseCondition "" "2 > fib(2)"
