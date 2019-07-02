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

data TypeError = Er String | Ok | Crt Type
      deriving (Eq,Show)

boolTypeError (Ok)= True
boolTypeError (Crt _ )= True
boolTypeError (Er _ )= False
getType:: TypeError -> Type
getType (Crt a) =  a
getType (Ok ) = SimplyNull
getType (Er _ ) = SimplyNull
stringTypeError (Er message ) = message
addMessage ::String -> TypeError ->TypeError
addMessage soure (Er message) = Er (message ++ " in " ++soure)
addMessage soure (Ok) = error "ok erro"
addMessage soure (Crt _ ) = Er soure

--String to Command List
fromStCL :: String -> [Commands]
fromStCL prog =fromBlock$ fromRight (Block [])  (parse parseBlock "" prog)

--this methods is used to get the current offset of a given scope 'x'
scopesTracker :: [(Int,Int)] ->Int ->Int
scopesTracker [] _ = error "Could not find offeset of scope"
scopesTracker ((a,b):xs) x | a == x = b
                           | otherwise = scopesTracker xs x
scopesTracker_test1 = scopesTracker [(1,0),(2,4)] 2
scopesTracker_test2 = scopesTracker [(1,0),(2,4)] 1

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
          (fromBlock ( fromRight (Block [])  treeBuilder_test1_AST))
          1 []
treeBuilder_test1_AST = parse parseBlock ""
      "{ global int z = 3 ;func int fib(int x, & int y){ int x =2 ;return y;}; int x = fib (ya, 2);func int fibi(int x, & int y){ int x =2 ;return y;};}"

treeBuilder_test2 = treeBuilder
          (fromBlock ( fromRight (Block [])  treeBuilder_test2_AST))
          1 []
treeBuilder_test2_AST = parse parseBlock ""
      "{ global int a = 0 ;global bool b = ya; int c = 1; bool d; global bool e;func void aux(){ int b = 0;}; }"



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
   getType$ typeExpr expr db
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
typeCheckProgram :: [Commands] -> [DataBase] -> [TypeError]
typeCheckProgram (x:prog) db  | boolTypeError$ typeCheck db x = typeCheckProgram prog db
                              | otherwise = typeCheck db x : typeCheckProgram prog db
typeCheckProgram [] _ = []

typeCheckProgram_test stg =
  typeCheckProgram (fromStCL stg) (treeBuilder (fromStCL stg) 1 [])

typeCheckProgram_test1 = typeCheckProgram_test "{ int x = 0; int y = 2; int z = x+y; print x;}"
typeCheckProgram_test2 = typeCheckProgram_test "{ int x = 0; int y = 2; int z = ya; print x;}"
typeCheckProgram_test3 = typeCheckProgram_test "{ while(ya >= 2){ int x;};}"
typeCheckProgram_test4 = typeCheckProgram_test "{ func int fib(int x,& int y){return 0}; int x = fib (2, nu);}"
aux= parse parseBlock " " "{ int x; bool y; x = 2 + ya; } "

--this metiods check is an individial command has the correct type
-- returns true if the types are correct and throws an error for different reasons
-- used in -> typeCheckProgram
typeCheck :: [DataBase] -> Commands -> TypeError
typeCheck db (VarDecl typ ex) | typeArgtype typ == (getType$ typeExpr ex db) = Ok
                              | otherwise = Er ("VarDecl " ++ stringArtgType typ ++ " wrong type assigned")
typeCheck db (GlobalVarDecl typ ex) | typeArgtype typ == (getType$ typeExpr ex db) = Ok
                              | otherwise = Er  ("GlobalVarDecl " ++ stringArtgType typ ++ " wrong type assigned")
typeCheck db fun@(FunCall name exprs) | findinDb name db == SimplyNull && boolTypeError (checkCorrectFuncCommand db fun)= Ok
                              | boolTypeError (checkCorrectFuncCommand db fun) = Er ("Funcall "++name ++ " is not null type")
                              | otherwise =  addMessage ("Funcall "++name) (checkCorrectFuncCommand db fun)
typeCheck db (Ass name ex) | findinDb name db == (getType$ typeExpr ex db) = Ok
                              | otherwise = addMessage ("Assigment "++ name) (typeExpr ex db)
typeCheck db (Decr name) | findinDb name db == SimplyInt = Ok
                              | otherwise = Er (name++" is not int, cannot --")
typeCheck db (Incr name) | findinDb name db == SimplyInt = Ok
                              | otherwise = Er (name++" is not int, cannot ++")
typeCheck db (AddCom name ex) | findinDb name db == (getType$ typeExpr ex db) = Ok
                              | not (boolTypeError$ typeExpr ex db) = addMessage ("Add Command "++ name) (typeExpr ex db)
                              | otherwise = Er ("Type error Add Command "++ name)
typeCheck db (MinCom name ex) | findinDb name db == (getType$ typeExpr ex db) = Ok
                              | not (boolTypeError$ typeExpr ex db) = addMessage ("Min Command "++ name) (typeExpr ex db)
                              | otherwise = Er ("Type error in Min Command "++ name)
typeCheck db (While cond _) | boolTypeError$ typeCheckCondition cond db = Ok
                            | otherwise = addMessage "While" (typeCheckCondition cond db)
typeCheck db (IfCom cond _ _) | boolTypeError$ typeCheckCondition cond db = Ok
                              | otherwise = addMessage "IfCom" (typeCheckCondition cond db)
typeCheck _ _ = Ok

typeCheck_test_vadecl1 = typeCheckProgram_test "{ int x = ya;}"
typeCheck_test_vadecl2 = typeCheckProgram_test "{ bool x = 2*3;}"
typeCheck_test_globalvadecl1 = typeCheckProgram_test "{ global int x = ya;}"
typeCheck_test_globalvadecl2 = typeCheckProgram_test "{ global bool x = 2+4*2;}"
typeCheck_test_funcall1 = typeCheckProgram_test "{ func int fib(int x){ int y; return y;}; fib(2);}"
typeCheck_test_funcall2 = typeCheckProgram_test "{ func void fib(int x){ int y;}; fib(2,3);}"
typeCheck_test_funcall3 = typeCheckProgram_test "{ func int fib(int x){ int y;return y; }; fib(2);}"
typeCheck_test_ass1 = typeCheckProgram_test "{ int x; bool y; x = 2 + ya; }"
typeCheck_test_ass2 = typeCheckProgram_test "{ int x; bool y; y = x;}"
typeCheck_test_incr = typeCheckProgram_test "{ bool x;x++;}"
typeCheck_test_decr = typeCheckProgram_test "{ bool x;x--;}"
typeCheck_test_addcom1 = typeCheckProgram_test "{ bool x; x+= 2+ya;}"
typeCheck_test_addcom2 = typeCheckProgram_test "{ int x; x+= ya;}"
typeCheck_test_mincom1 = typeCheckProgram_test "{ bool x; x-= 2+ya;}"
typeCheck_test_mincom2 = typeCheckProgram_test "{ int x; x-= ya;}"
typeCheck_test_while1 = typeCheckProgram_test "{ while(ya >= 2){ int x;};}"
typeCheck_test_ifcomand = typeCheckProgram_test "{ if(ya >= 2){ int x;}{};}"


-- this methdos takes and expression and the database and returns the expression's type
-- used in -> typeCheck, exprTypeFromRet, typeCheckCondition
-- if the exprssions withing the argument expresson are wrong, an appropriete error message will arise
typeExpr :: Expr -> [DataBase]-> TypeError
typeExpr (Constant _ ) db =Crt SimplyInt
typeExpr (BoolConst _ ) db = Crt SimplyBol
typeExpr (Mult e1 e2 ) db | not (t1 == t2) = Er "Mutiplication elements are not the same type"
                          | t1 ==Crt  SimplyInt =Crt  SimplyInt
                          | t1 ==Crt  SimplyBol = Er "Multiplication of bools not allowed"
        where
          t1 = typeExpr e1 db
          t2 = typeExpr e2 db
typeExpr (Add e1 e2 ) db | not (t1 == t2) = Er "Addition elemets are not the same type"
                       | t1 == Crt SimplyInt =Crt SimplyInt
                       | t1 ==Crt  SimplyBol = Er "Addition of bools not allowed"
        where
          t1 = typeExpr e1 db
          t2 = typeExpr e2 db
typeExpr (Paren x ) db = typeExpr x db
typeExpr (Min e1 e2 ) db | not (t1 == t2) = Er "Substractions elemets are not the same type"
                       | t1 == Crt SimplyInt = Crt SimplyInt
                       | t1 == Crt SimplyBol = Er "subtraction of bools not allowed"
        where
          t1 = typeExpr e1 db
          t2 = typeExpr e2 db
typeExpr (IfExpr typ cond _ _ ) db | boolTypeError$ typeCheckCondition cond db =Crt  typ
                                   | otherwise = Er "Condition types not the same in IfExpr"
typeExpr (Identifier x ) db = Crt$ findinDb x db
typeExpr fun@(Funct name exprs) db | checkCorrectFuncExpr db fun = Crt$ findinDb name db
                                   | otherwise = Er "Function's arguments are not correct in FuncExpr"

typeExpr _ _ =Crt  SimplyNull


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
typeCheckCondition :: Condition -> [DataBase] -> TypeError
typeCheckCondition (Eq e1 e2)  db | typeExpr e1 db == typeExpr e2 db = Ok
                                  | otherwise =  Er "Not same type in Eq Condtion"
typeCheckCondition (Lt e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db == Crt SimplyBol) = Ok
                                  | typeExpr e1 db == typeExpr e2 db = Er "Bools cannot be >"
                                  | otherwise =  Er "Not same type in Lt Condtion"
typeCheckCondition (Gt e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db == Crt SimplyBol) = Ok
                                  | typeExpr e1 db == typeExpr e2 db = Er "Bools cannot be <"
                                  | otherwise =  Er "Not same type in Gt Condtion"
typeCheckCondition (Lq e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db ==Crt  SimplyBol) = Ok
                                  | typeExpr e1 db == typeExpr e2 db = Er "Bools cannot be <="
                                  | otherwise =  Er "Not same type in Lq Condtion"
typeCheckCondition (Gq e1 e2)  db | typeExpr e1 db == typeExpr e2 db && not (typeExpr e1 db ==Crt  SimplyBol) = Ok
                                  | typeExpr e1 db == typeExpr e2 db = Er "Bools cannot be >="
                                  | otherwise =  Er "Not same type in Gq Condtion"

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
comparaParamsandArgs db (para:params) (e:exprs) | getParamType para == (getType$typeExpr e db)
                                        = comparaParamsandArgs db params exprs
                          | otherwise = False


checkCorrectFuncExpr :: [DataBase] -> Expr -> Bool
checkCorrectFuncExpr db (Funct name args ) = comparaParamsandArgs db (findMethodParamsDB db name) args

checkCorrectFuncCommand :: [DataBase] -> Commands-> TypeError
checkCorrectFuncCommand db (FunCall name args ) | comparaParamsandArgs db (findMethodParamsDB db name) args = Ok
                                           | otherwise = Er "Function's arguments are not correct"
























--eof
