module TreeWalker where

import Parser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- import Data



-- typeChecker :: Bloc
-- typeChecker (Bloc (VarDecl (Bol name) ) )

data Function = DBFunction ArgType [ArgType]
    deriving Show

data DataBase = DBBruh Int ArgType


fromBlock :: Bloc -> [Commands]
fromBlock (Block xs) = xs


typeCheck :: Commands -> [DataBase]-> Bool
typeCheck (VarDecl typ ex) db  =fromArgtype typ == typeExpr ex db
typeCheck (GlobalVarDecl typ ex) db  =fromArgtype typ == typeExpr ex db
-- typeCheck (Ass name ex) db  = findinDb name db == typeExpr ex db
-- typeCheck (Decr name db)  = findinDb name db == SimplyInt
-- typeCheck (Incr name db)  = findinDb name db == SimplyInt
-- typeCheck (AddCom name ex) db  = findinDb name db == typeExpr ex db
-- typeCheck (MinCom name ex) db  = findinDb name db == typeExpr ex db

fromArgtype:: ArgType -> IfType
fromArgtype (Bol _ ) = SimplyBol
fromArgtype (Int _ ) = SimplyInt


typeExpr :: Expr -> [DataBase]-> IfType
typeExpr (Constant _ ) db = SimplyInt
typeExpr (BoolConst _ ) db = SimplyBol
typeExpr (Mult _ _ ) db = SimplyInt
typeExpr (Add _ _ ) db = SimplyInt
typeExpr (Paren x ) db = typeExpr x db
typeExpr (Min _ _ ) db = SimplyInt
typeExpr (IfExpr typ _ _ _ ) db = typ
-- typeExpr (Identifier x ) db = findinDb x db     --TO DO
-- typeExpr (Funct name _) = findinDb name db --TO DO

typeCheckCondition :: Condition -> [DataBase] -> Bool
typeCheckCondition (Lt e1 e2)  db = typeExpr e1 db == typeExpr e2 db
typeCheckCondition (Eq e1 e2)  db = typeExpr e1 db == typeExpr e2 db
typeCheckCondition (Gt e1 e2)  db = typeExpr e1 db == typeExpr e2 db
typeCheckCondition (Lq e1 e2)  db = typeExpr e1 db == typeExpr e2 db
typeCheckCondition (Gq e1 e2)  db = typeExpr e1 db == typeExpr e2 db





-- scopes :: Bloc -> [[ArgType]]
-- scopes (Block xs) db =

-- scopes :: Bloc -> [[ArgType]]
-- scopes x = []
-- scopes block = p args
--       where
--         arg = fromBlock block
--         p ((VarDecl arg expr ):xs) =




functions :: [Commands] -> [Function]
functions [] = []
functions ((FunDecl a args irel):xs) = (DBFunction a args) : functions xs
functions (x:xs) = functions xs
functions_test1 = functions []






























--eof
