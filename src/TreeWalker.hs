module TreeWalker where

import Parser
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
-- import Data



-- typeChecker :: Bloc
-- typeChecker (Bloc (VarDecl (Bol name) ) )

data Function = DBFunction ArgType [Param]
    deriving Show

data DataBase = DB ArgType Int Int | DBF ArgType [Param] Bloc
    -- //first int scopes, second int is scope offset, third i offest within scope
      deriving (Eq,Show)


fromBlock :: Bloc -> [Commands]
fromBlock (Block xs) = xs

scopesTracker :: [(Int,Int)] ->Int ->Int
scopesTracker [] _ = error "Could not find offeset of scope"
scopesTracker ((a,b):xs) x | a == x = b
                           | otherwise = scopesTracker xs x
scopesTracker_test1 = scopesTracker [(1,0),(2,4)] 2

increaseOffset:: [(Int, Int)] -> Int -> [(Int, Int)]
increaseOffset [] x = [(x,0)]
increaseOffset ((a,b):xs) x | a == x = (a,(b+4)):xs
                            | otherwise = (a,b):increaseOffset xs x
increaseOffset_test1 = increaseOffset [(1,0),(2,4)] 2
increaseOffset_test2 = increaseOffset [] 2




treeBuilder :: [Commands] -> Int ->[(Int, Int)] -> [DataBase]
treeBuilder [] _ _ = []
treeBuilder ((VarDecl arg expr):xs) scope off | checkDuplicant db arg scope = add:db
                                              | otherwise = error "Dupicant declaration in same scope "
    where
      db = treeBuilder xs scope (increaseOffset off scope)
      add = (DB arg scope (scopesTracker off scope))
treeBuilder ((GlobalVarDecl arg expr):xs) scope off | checkDuplicant db arg scope = add:db
                                                    | otherwise = error "Dupicant declaration in same scope "
    where
      db = treeBuilder xs scope (increaseOffset off scope)
      add = (DB arg 0 (scopesTracker off scope))
        -- (DB arg 0 (scopesTracker off scope)): treeBuilder xs scope (increaseOffset off scope)

treeBuilder ((FunDecl arg args bloc):xs) scope off | checkDuplicant db arg scope = add:ownscope ++ db
                                                   | otherwise = error "Dupicant Method in program "
    where
      db = treeBuilder xs scope off
      ownscope = treeBuilder (fromBlock bloc) (scope+1) (increaseOffset off (scope+1))
      add = (DBF arg args bloc)
          -- (DB arg 0 0): treeBuilder (fromBlock bloc) (scope+1) (increaseOffset off (scope+1))
          --  ++ treeBuilder xs scope off
treeBuilder ((Fork bloc):xs) scope off =
    treeBuilder (fromBlock bloc) (scope+1) off ++ treeBuilder xs scope off
treeBuilder (x:xs) scope off = treeBuilder xs scope off


treeBuilder_test1 = treeBuilder
          (fromBlock ( fromRight (Block [])
          aux))
          1 [(1,0)]
aux = parse parseBlock ""
      "{ global int a =2 ;int b =3; func int fib (int x){\n int x = 0;int a = 2;}; func int fib(int x) {};}; }"


checkDuplicant :: [DataBase]-> ArgType ->Int -> Bool
checkDuplicant [] _ _ = True
checkDuplicant (( DB dbarg sco _):xs) arg scope
      | scope == sco && stringArtgType dbarg == stringArtgType arg = False
      | otherwise = checkDuplicant xs arg scope
checkDuplicant ((DBF name params bloc):xs) arg scope
      | stringArtgType name == stringArtgType arg = False
      | otherwise = checkDuplicant xs arg scope












typeCheck :: Commands -> [DataBase]-> Bool
typeCheck (VarDecl typ ex) db  =fromArgtype typ == typeExpr ex db
typeCheck (GlobalVarDecl typ ex) db  =fromArgtype typ == typeExpr ex db
-- typeCheck (FunCall name exprs) db  = findinDb name db == SimplyNull --fun calls can only be done on void methods
-- typeCheck (Ass name ex) db  = findinDb name db == typeExpr ex db
-- typeCheck (Decr name db)  = findinDb name db == SimplyInt
-- typeCheck (Incr name db)  = findinDb name db == SimplyInt
-- typeCheck (AddCom name ex) db  = findinDb name db == typeExpr ex db
-- typeCheck (MinCom name ex) db  = findinDb name db == typeExpr ex db

fromArgtype:: ArgType -> Type
fromArgtype (Arg t _ ) = t


-- findinDb:: String -> [DataBase] -> Type
-- findinDb name db |

stringArtgType :: ArgType ->  String
stringArtgType (Arg _ x ) = x

typeExpr :: Expr -> [DataBase]-> Type
typeExpr (Constant _ ) db = SimplyInt
typeExpr (BoolConst _ ) db = SimplyBol
typeExpr (Mult e1 e2 ) db | not (t1 == t2) = error "Mutiplication elements are not the same type"
                          | t1 == SimplyInt = SimplyInt
                          | t1 == SimplyBol = error "Multiplication of bools not allowed"
        where
          t1 = typeExpr e1 db
          t2 = typeExpr e2 db
typeExpr (Add e1 e2 ) db | not (t1 == t2) = error "Addition elemets are not the same type"
                       | t1 == SimplyInt = SimplyInt
                       | t1 == SimplyBol = error "Addition of bools not allowed"
        where
          t1 = typeExpr e1 db
          t2 = typeExpr e2 db
typeExpr (Paren x ) db = typeExpr x db
typeExpr (Min e1 e2 ) db | not (t1 == t2) = error "Substractions elemets are not the same type"
                       | t1 == SimplyInt = SimplyInt
                       | t1 == SimplyBol = error "subtraction of bools not allowed"
        where
          t1 = typeExpr e1 db
          t2 = typeExpr e2 db
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




-- functions :: [Commands] -> [Function]
-- functions [] = []
-- functions ((FunDecl a args irel):xs) = (DBFunction a args) : functions xs
-- functions (x:xs) = functions xs
-- functions_test1 = functions []






























--eof
