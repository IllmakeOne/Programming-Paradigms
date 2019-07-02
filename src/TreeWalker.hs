module TreeWalker where

import Parser
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Structure





--the [DataBasse] is the use as a Symbol Table of sorts
-- it has two type
--  DB which is for the variables of the program,
--    it keeps track of the variable's name and type in ArgType
--    and two integers, the first one is the scope of the vribale the second is the offset
-- DBF which is for the functions declared in the program
--    it keeps track of the method's name and type in ArgType and it's parameters
data DataBase = DB ArgType Int Int | DBF ArgType [Param] Bloc
      deriving (Eq,Show)

--String to Command List
fromStCL :: String -> [Commands]
fromStCL prog =fromBlock$ fromRight (Block [])  (parse parseBlock "" prog)

--this methods is used to get the current offset of a given scope 'x'
scopesTracker :: [(Int,Int)] ->Int ->Int
scopesTracker [] _ = error "Could not find offeset of scope"
scopesTracker ((a,b):xs) x | a == x = b
                           | otherwise = scopesTracker xs x
scopesTracker_test1 = scopesTracker [(1,0),(2,4)] 2

--this methods is used to increment the offest of a scope 'x'
--if the scope is not declared yet, declare it with offset 0
increaseOffset:: [(Int, Int)] -> Int -> [(Int, Int)]
increaseOffset [] x = [(x,0)]
increaseOffset ((a,b):xs) x | a == x = (a,(b+1)):xs
                            | otherwise = (a,b):increaseOffset xs x
increaseOffset_test1 = increaseOffset [(1,0),(2,4)] 2
increaseOffset_test2 = increaseOffset [] 2


--this methods creates the DataBase/symbol table list
--  which would be used for type checking and in computing
-- it's second argument, the int , is the scope in which the method is right now
-- it's second argument should be initiallized with 1, as 0 is reserved for global varaibles
-- and it's third argument should just be an empty list
treeBuilder :: [Commands] -> Int ->[(Int, Int)] -> [DataBase]
treeBuilder [] _ _ = []
treeBuilder ((VarDecl arg expr):xs) scope off | checkDuplicant db arg scope = add:db
                                              | otherwise = error "Dupicant declaration in same scope "
    where
      db = treeBuilder xs scope (increaseOffset off scope)
      add = (DB arg scope (scopesTracker (increaseOffset off scope) scope))
treeBuilder ((GlobalVarDecl arg expr):xs) scope off | checkDuplicant db arg 0 = add:db
                                                    | otherwise = error "Dupicant global declaration "
    where
      db = treeBuilder xs scope (increaseOffset off 0)
      add = (DB arg 0 (scopesTracker (increaseOffset off 0) 0))
        -- (DB arg 0 (scopesTracker off scope)): treeBuilder xs scope (increaseOffset off scope)

treeBuilder ((FunDecl arg args bloc):xs) scope off | checkDuplicant db arg scope && typeArgtype arg == ret = add:ownscope ++ db
                                                   | typeArgtype arg == ret = error "Dupicant Method in program "
                                                   | otherwise = error "Return type of methods not same type ans methods type "
    where
      db = treeBuilder xs scope off
      ownscope = treeBuilder (fromBlock bloc) (scope+1) (increaseOffset off (scope+1))
      add = (DBF arg args bloc)
      blocReturn = (findRetinBloc $ fromBlock bloc) --the return command of the method
      ret | typeArgtype arg == SimplyNull && blocReturn == (Return NullExpr) = SimplyNull
          | typeArgtype arg == SimplyNull && not (blocReturn == (Return NullExpr)) = error "Void methods has return"
          | otherwise = exprTypeFromRet (traceShowId (onlyGlobals db)) args bloc blocReturn
      --ret is the return type of the methods being declared

treeBuilder ((Fork bloc):xs) scope off =
    treeBuilder (fromBlock bloc) (scope+1) off ++ treeBuilder xs scope off
treeBuilder (x:xs) scope off = treeBuilder xs scope off

treeBuilder_test1 = treeBuilder
          (fromBlock ( fromRight (Block [])  aux))
          1 []
aux = parse parseBlock ""
      "{ global int z = 3 ;func int fib(int x, & int y){ int x =2 ;return y;}; int x = fib (ya, 2);func int fibi(int x, & int y){ int x =2 ;return y;};}"

--this methods seraches for the return in a list of commands
-- used in -> treeBuilder for fundecl for checking if a method has the corret  return type
findRetinBloc :: [Commands] -> Commands
findRetinBloc [] = (Return NullExpr)
findRetinBloc ((Return expr):xs) = (Return expr)
findRetinBloc (x:xs) = findRetinBloc xs

--this method adds params by ref to the DataBase,
--    so if the return of a method calls its byref param it will not error
-- methods used in -> exprTypeFromRet
addByrefParamsDb :: [Param] -> Int ->[(Int, Int)]  -> [DataBase]
addByrefParamsDb [] _ _ = []
addByrefParamsDb ((ByVal _):ps) scope off = addByrefParamsDb ps scope off
addByrefParamsDb ((ByRef arg):ps) scope off =
      (DB arg (scopesTracker (increaseOffset off scope) scope) scope):addByrefParamsDb ps scope (increaseOffset off (scope+1))
addByrefParamsDb_tes1 = addByrefParamsDb [ByVal (Arg SimplyInt "x"),ByRef (Arg SimplyInt "y")] 1 [(0,0)]

--method that puts together the current database and the DB entries of a blok of a methods, and the methods;s parameters
--methods used in ->  treeBuilder for fundecl for checking if a method has the corret  return type
exprTypeFromRet ::[DataBase] -> [Param] ->Bloc -> Commands -> Type
exprTypeFromRet globalDb param bloc (Return expr) =
   typeExpr expr db
   where
     paramdb = addByrefParamsDb param 0 []
     blocdb = treeBuilder (fromBlock bloc) 0 []
     db = paramdb ++ blocdb ++ globalDb

--methods taht filters out all the non global variables
-- used in -> treeBuilder for fundecl for checking if a method has the corret  return type
onlyGlobals:: [DataBase] -> [DataBase]
onlyGlobals [] = []
onlyGlobals ((DB arg 0 off):db) =(DB arg 0 off) : onlyGlobals db
onlyGlobals (x:db) = onlyGlobals db
-- onlyGlobals_test1 =

--helped method that return the scope and offset of a varaibles based on its name
-- not used anywhere at the moment of writing this comment
getOffset :: [DataBase] -> String -> (Int, Int)
getOffset [] _ = error "getOffset error"
getOffset (DB (Arg argType argName) x y:xs) name
      | argName == name = (x, y)
      | otherwise = getOffset xs name


--this methods checks if a variable/method has been declared before
--variables are compared to varaibles withing the same scope and with global variables
-- global variables are compared just wiht other global varaibles
checkDuplicant :: [DataBase]-> ArgType ->Int -> Bool
checkDuplicant [] _ _ = True
checkDuplicant (( DB dbarg sco _):xs) arg scope
      | scope == sco && stringArtgType dbarg == stringArtgType arg = False
      | 0 == sco && stringArtgType dbarg == stringArtgType arg = error "Var with same name as global variable"
      | otherwise = checkDuplicant xs arg scope
checkDuplicant ((DBF name params _):xs) arg scope
      | stringArtgType name == stringArtgType arg = False
      | otherwise = checkDuplicant xs arg scope


--------------------------------TypeChecking-------------------------------------

-- main type checking metods, takes a list of commands and the symbol table
-- returns true if all types are correct and throws exceptions for wrong types
typeCheckProgram :: [Commands] -> [DataBase] -> Bool
typeCheckProgram (x:prog) db  | typeCheck db x = typeCheckProgram prog db
                              | otherwise = error "Tyepe chekcpr porgram eror somehow"
typeCheckProgram [] _ = True

typeCheckProgram_test stg =
  typeCheckProgram (fromStCL stg) (treeBuilder (fromStCL stg) 1 [])

typeCheckProgram_test1 = typeCheckProgram_test "{ int x = 0; int y = 2; int z = x+y; print x;}"
typeCheckProgram_test2 = typeCheckProgram_test "{ int x = 0; int y = 2; int z = ya; print x;}"
typeCheckProgram_test3 = typeCheckProgram_test "{ func int fib(int x,& int y){}; int x = fib (2, 2);}"
typeCheckProgram_test4 = typeCheckProgram_test "{ func int fib(int x,& int y){return 0}; int x = fib (2, nu);}"


--this metiods check is an individial command has the correct type
-- returns true if the types are correct and throws an error for different reasons
-- used in -> typeCheckProgram
typeCheck :: [DataBase] -> Commands -> Bool
typeCheck db (VarDecl typ ex) | typeArgtype typ == typeExpr ex db = True
                              | otherwise = error "Type error in type declarations"
typeCheck db (GlobalVarDecl typ ex) | typeArgtype typ == typeExpr ex db = True
                              | otherwise = error "Type error in global type declarations"
typeCheck db fun@(FunCall name exprs) | findinDb name db == SimplyNull && checkCorrectFuncCommand db fun= True
                              | otherwise = error "Type error in fucntion call"
typeCheck db (Ass name ex) | findinDb name db == typeExpr ex db = True
                              | otherwise = error "Type error in variable assignment"
typeCheck db (Decr name) | findinDb name db == SimplyInt = True
                              | otherwise = error "Type error in variable decrese '--' "
typeCheck db (Incr name) | findinDb name db == SimplyInt = True
                              | otherwise = error "Type error in variable increaase '++' "
typeCheck db (AddCom name ex) | findinDb name db == typeExpr ex db = True
                              | otherwise = error "Type error in add command += "
typeCheck db (MinCom name ex) | findinDb name db == typeExpr ex db = True
                              | otherwise = error "Type error in minus commdn -= "
typeCheck db (While cond _) | typeCheckCondition cond db = True
                          | otherwise = error "Cond var not the same type in While"
typeCheck db (IfCom cond _ _) | typeCheckCondition cond db = True
                              | otherwise = error "Cond var not the same type in IfCom"
typeCheck _ _ = True


-- this methdos takes and expression and the database and returns the expression's type
-- used in -> typeCheck, exprTypeFromRet, typeCheckCondition
-- if the exprssions withing the argument expresson are wrong, an appropriete error message will arise
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
typeExpr (IfExpr typ cond _ _ ) db | typeCheckCondition cond db = typ
                                   | otherwise = error "Condition types not the same in IfExpr"
typeExpr (Identifier x ) db = findinDb x db
typeExpr fun@(Funct name exprs) db | checkCorrectFuncExpr db fun = findinDb name db
                                   | otherwise = error "Function's arguments are not correct in FuncExpr"


--this methods looks in the database for a namse and returns its type
-- used in determining a exprssion's type -> typeExpr
findinDb:: String -> [DataBase] -> Type
findinDb name ((DB  arg _ _):dbx) | name == stringArtgType arg = typeArgtype arg
                                  | otherwise = findinDb name dbx
findinDb name ((DBF  arg  _ _):dbx) | name == stringArtgType arg = typeArgtype arg
                                  | otherwise = findinDb name dbx
findinDb _ [] = error "undeclared variable called"


--checks if the expression in a condition are the same type
-- returns true if they are and false if they are no the same type
typeCheckCondition :: Condition -> [DataBase] -> Bool
typeCheckCondition (Eq e1 e2)  db = typeExpr e1 db == typeExpr e2 db
typeCheckCondition (Lt e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db == SimplyBol) = True
                                  | typeExpr e1 db == typeExpr e2 db = error "Bools cannot only be =="
typeCheckCondition (Gt e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db == SimplyBol) = True
                                  | typeExpr e1 db == typeExpr e2 db = error "Bools cannot only be =="
typeCheckCondition (Lq e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db == SimplyBol) = True
                                  | typeExpr e1 db == typeExpr e2 db = error "Bools cannot only be =="
typeCheckCondition (Gq e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db == SimplyBol) = True
                                  | typeExpr e1 db == typeExpr e2 db = error "Bools cannot only be =="

getParamType :: Param -> Type
getParamType (ByVal (Arg x _)) =  x
getParamType (ByRef (Arg x _)) =  x


findMethodParamsDB :: [DataBase] -> String -> [Param]
findMethodParamsDB ((DBF fname params _):db) name | stringArtgType fname == name = params
                                                | otherwise = findMethodParamsDB db name
findMethodParamsDB ((DB _ _ _):db) name = findMethodParamsDB db name
findMethodParamsDB [] _ = error "Could not find methods params ind findMethodParamsDB"

comparaParamsandArgs :: [DataBase] -> [Param] -> [Expr] -> Bool
comparaParamsandArgs _ [] [] = True
comparaParamsandArgs _ [] _ = False
comparaParamsandArgs _ _ [] = False
comparaParamsandArgs db (para:params) (e:exprs) | getParamType para == typeExpr e db
                                        = comparaParamsandArgs db params exprs
                          | otherwise = False


checkCorrectFuncExpr :: [DataBase] -> Expr -> Bool
checkCorrectFuncExpr db (Funct name args ) = comparaParamsandArgs db (findMethodParamsDB db name) args

checkCorrectFuncCommand :: [DataBase] -> Commands-> Bool
checkCorrectFuncCommand db (FunCall name args ) | comparaParamsandArgs db (findMethodParamsDB db name) args = True
                                           | otherwise = error "Function's arguments are not correct in FuncCommand"
























--eof
