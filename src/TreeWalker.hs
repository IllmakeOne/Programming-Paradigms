module TreeWalker where

import Parser
import Debug.Trace
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Structure





--the [DataBasse] is the use as a Symbol Table of sorts
-- it has three type
--  DB which is for the variables of the program,
--    it keeps track of the variable's name and type in ArgType
--    and two integers, the first one is the scope of the vribale the second is the offset
-- DBF which is for the functions declared in the program
--    it keeps track of the method's name and type in ArgType and it's parameters
-- Err is used to stored errors that are present in the AST being analised
data DataBase = DB ArgType Int Int | DBF ArgType [Param] Bloc | Err ArgType TypeError
      deriving (Eq,Show)


--this methods takes a String and returns a Bool string touple
-- the bool is true if there are no errors found in the typechecking and the symbol table creation
-- if it is falls, the erros that were found are stores
checkCorrectProgram :: String -> (Bool,[DataBase])
checkCorrectProgram stg | not$ showErrorinDB dbp == [] = (False, showErrorinDB dbp)
                        | otherwise = (True, [])
    where
      db = symbolTableBuilder_fromStg stg
      tcheck = typeCheckProgram (fromStCL stg) db
      dbp = addTypeErrstoDB tcheck db



checkCorrectProgram_report = checkCorrectProgram "{ int x =2; bool x = ya ; func int fib (int x) { print x;}; func int other (int x) { return ya;}; fib (2);}"
      ==(False,[Err (Arg SimplyNull "") (Er "No params because didnt find method "),Err (Arg SimplyInt "x") (Er "Duplicate declaration in same scope"),Err (Arg SimplyInt "fib") (Er "Method has wrong return or no return "),Err (Arg SimplyInt "other") (Er "Return type of method not same type and method's type ")])

--------------------------------------------------------------------------------
--------------------------------DataBase Creation-------------------------------------
--------------------------------------------------------------------------------

--String to Command List
fromStCL :: String -> [Commands]
fromStCL prog | isLeft parsed = error "Not parsed correctly"
              | otherwise = fromBlock$ fromRight (Block [])  (parse parseBlock "" prog)
  where
    parsed =parse parseBlock "" prog
fromStCL_test = fromStCL "{ func int fib (int x) { print x;}; fib(2); }" ==
                    [FunDecl (Arg SimplyInt "fib") [ByVal (Arg SimplyInt "x")] (Block [Print (Identifier "x"),End]),FunCall "fib" [Constant 2],End]

--this methods creates the DataBase/symbol table list
--  which would be used for type checking and in computing
-- it's second argument, the int , is the scope in which the method is right now
-- it's second argument should be initiallized with 1, as 0 is reserved for global varaibles
-- and it's third argument should just be an empty list
symbolTableBuilder :: [Commands] -> Int ->[(Int, Int)] -> [DataBase]
symbolTableBuilder [] _ _ = []
symbolTableBuilder ((VarDecl arg expr):xs) scope off | boolTypeError dupTest = add:db
                                                     | otherwise = (Err arg dupTest ):db
    where
      dupTest= checkDuplicant db arg scope
      db = symbolTableBuilder xs scope (increaseOffset off scope)
      add = (DB arg scope (scopesTracker (increaseOffset off scope) scope))
symbolTableBuilder ((GlobalVarDecl arg expr):xs) scope off |boolTypeError dupTest = add:db
                                                    | otherwise = (Err arg dupTest ):db
    where
      dupTest= checkDuplicant db arg 0
      db = symbolTableBuilder xs scope (increaseOffset off 0)
      add = (DB arg 0 (scopesTracker (increaseOffset off 0) 0))
        -- (DB arg 0 (scopesTracker off scope)): symbolTableBuilder xs scope (increaseOffset off scope)

symbolTableBuilder ((FunDecl arg param bloc):xs) scope off
                              | boolTypeError dupTest == False =(Err arg dupTest) : db
                              | boolTypeError ret == False = Err arg ret  :db
                              | typeArgtype arg == getType ret = add:ownscope ++ db
                              | not(typeArgtype arg == SimplyNull) && getType ret == SimplyNull
                                          -- = Err arg (Er  "Return type of methods not same type and methods type "):db
                                          = (Err arg (Er "Method has wrong return or no return ")) : db
                              | otherwise = Err arg (Er  "Return type of method not same type and method's type "):db
    where
      dupTest= checkDuplicant db arg scope
      db = symbolTableBuilder xs scope off
      ownscope = symbolTableBuilder (((paramtoCom param )++ fromBlock bloc)) (scope+1) (increaseOffset off (scope+1))
      add = (DBF arg param bloc)
      blocReturn = (findRetinBloc $ fromBlock bloc) --the return command of the method
      ret | blocReturn == (Return NullExpr) = Crt SimplyNull
          | typeArgtype arg == SimplyNull && not (blocReturn == (Return NullExpr)) = Er "Void method with return"
          | otherwise = Crt$ exprTypeFromRet (onlyGlobals db) param bloc blocReturn
      --ret is the return type of the methods being declared

symbolTableBuilder ((While _ bloc):xs) scope off = db
    where
      db = symbolTableBuilder (fromBlock bloc ++ xs) scope off

symbolTableBuilder ((IfCom _ bloc1 bloc2):xs) scope off = db
    where
      db = symbolTableBuilder (fromBlock bloc1 ++ fromBlock bloc2 ++ xs) scope off

symbolTableBuilder (x:xs) scope off = symbolTableBuilder xs scope off

symbolTableBuilder_while_test = symbolTableBuilder_fromStg "{ int x; while(x<10){ int y; int q; };int z;int k; }"
symbolTableBuilder_if_test = symbolTableBuilder_fromStg "{ int x; if(x<10){ int y; int q; }{ int l;};int z;int k; }"

-- auxiliary method used in -> symbolTableBuilder FuncDecl case
--  it creates variable declaration commnad out fo the parameters
--so they can be easily introduced in the scope of the methods
paramtoCom :: [Param] -> [Commands]
paramtoCom [] = []
paramtoCom ((ByVal arg):xs) = VarDecl arg NullExpr : paramtoCom xs
paramtoCom ((ByRef arg):xs) = VarDecl arg NullExpr : paramtoCom xs



--checks if by referece parameters are not simple values
checkRefParam :: [Param] -> [Expr]-> TypeError
checkRefParam [] [] = Ok
checkRefParam ((ByRef (Arg typ name)):ps) ((Identifier _):ex) = checkRefParam ps ex
checkRefParam ((ByRef _):ps) (_:ex) = Er "Param by referece is given an expression"
checkRefParam ((ByVal _):ps) (_:ex) = checkRefParam ps ex
checkRefParam _ [] = Er "Fewer arguments than parameters"
checkRefParam [] _ = Er "Fewer parameters than arguments"
-- checkRefParam_test = checkRefParam [ByRef (Arg SimplyInt "x"),ByVal (Arg SimplyInt "y"),ByVal (Arg SimplyInt "z")]

--this methods takes a string , parses it and then creates the symbol table for it, withtout the type errros
symbolTableBuilder_fromStg :: String -> [DataBase]
symbolTableBuilder_fromStg string | isLeft parsed = error "Not parsed correctly "
                                  | otherwise = symbolTableBuilder (fromBlock ( fromRight (Block [])  parsed)) 1 []
     where
       parsed = (parse parseBlock "" string)


--this methods is used to get the current offset of a given scope 'x'
scopesTracker :: [(Int,Int)] ->Int ->Int
scopesTracker [] _ = error "Could not find offeset of scope"
scopesTracker ((a,b):xs) x | a == x = b
                           | otherwise = scopesTracker xs x
scopesTracker_test1 = scopesTracker [(1,0),(2,4)] 2 == 4
scopesTracker_test2 = scopesTracker [(1,0),(2,4)] 1 == 0

--this methods is used to increment the offest of a scope 'x'
--if the scope is not declared yet, declare it with offset 0
increaseOffset:: [(Int, Int)] -> Int -> [(Int, Int)]
increaseOffset [] x = [(x,0)]
increaseOffset ((a,b):xs) x | a == x = (a,(b+1)):xs
                            | otherwise = (a,b):increaseOffset xs x
increaseOffset_test1 = increaseOffset [(1,0),(2,4)] 2 == [(1,0),(2,5)]
increaseOffset_test2 = increaseOffset [] 2  == [(2,0)]

--this methods seraches for the return in a list of commands
-- used in -> symbolTableBuilder for fundecl for checking if a method has the corret  return type
findRetinBloc :: [Commands] -> Commands
findRetinBloc [] = (Return NullExpr)
findRetinBloc ((Return expr):xs) = (Return expr)
findRetinBloc (x:xs) = findRetinBloc xs

--this method adds params by ref to the DataBase,
--    so if the return of a method calls its byref param it will not error
-- methods used in -> exprTypeFromRet
addByrefParamsDb :: [Param] -> Int ->[(Int, Int)]  -> [DataBase]
addByrefParamsDb [] _ _ = []
addByrefParamsDb ((ByVal arg):ps) scope off =
      (DB arg (scopesTracker (increaseOffset off scope) scope) scope):addByrefParamsDb ps scope (increaseOffset off (scope+1))
addByrefParamsDb ((ByRef arg):ps) scope off =
      (DB arg (scopesTracker (increaseOffset off scope) scope) scope):addByrefParamsDb ps scope (increaseOffset off (scope+1))
addByrefParamsDb_tes1 = addByrefParamsDb [ByVal (Arg SimplyInt "x"),ByRef (Arg SimplyInt "y")] 1 [(0,0)]


--method that puts together the current database and the DB entries of the block of a methods, and the methodss parameters
--methods used in ->  symbolTableBuilder for fundecl for checking if a method has the corret  return type
--this is done because in some cases the return is one fo the parameters
exprTypeFromRet ::[DataBase] -> [Param] ->Bloc -> Commands -> Type
exprTypeFromRet globalDb param bloc (Return expr) =
   getType$ typeExpr expr db
   where
     paramdb = addByrefParamsDb param 0 []
     blocdb = symbolTableBuilder (fromBlock bloc) 0 []
     db = paramdb ++ blocdb ++ globalDb


--methods taht filters out all the non global variables
-- used in -> symbolTableBuilder for fundecl for checking if a method has the corret  return type
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
checkDuplicant :: [DataBase]-> ArgType ->Int -> TypeError
checkDuplicant [] _ _ = Ok
checkDuplicant (( DB dbarg sco _):xs) arg scope
      | scope == sco && stringArtgType dbarg == stringArtgType arg = Er "Duplicate declaration in same scope"
      | 0 == sco && stringArtgType dbarg == stringArtgType arg = Er "Var with same name as global variable"
      | otherwise = checkDuplicant xs arg scope
checkDuplicant ((DBF name params _):xs) arg scope
      | stringArtgType name == stringArtgType arg = Er "Duplicate declaration of method"
      | otherwise = checkDuplicant xs arg scope
checkDuplicant ((Err _ _):xs) arg scope = checkDuplicant xs arg scope







--------------------------------------------------------------------------------
--------------------------------TypeChecking-------------------------------------
--------------------------------------------------------------------------------

-- main type checking metods, takes a list of commands and the symbol table
-- returns a list of erros, if the list is empty, then the progrma is correct
typeCheckProgram :: [Commands] -> [DataBase] -> [TypeError]
typeCheckProgram (x:prog) db  | boolTypeError$ typeCheck db x = typeCheckProgram prog db
                              | otherwise = typeCheck db x : typeCheckProgram prog db
typeCheckProgram [] _ = []

-- takes a string , parses it, creates a Symbol Table for it and then it runs the type checking
runChecksfromString stg =
  typeCheckProgram (fromStCL stg) (symbolTableBuilder (fromStCL stg) 1 [])


--this metiods check is an individial command has the correct type
-- returns true if the types are correct and throws an error for different reasons
-- used in -> typeCheckProgram
typeCheck :: [DataBase] -> Commands -> TypeError
typeCheck db (VarDecl typ ex) | typeArgtype typ == (getType exprt) = Ok
                              | boolTypeError exprt =
                                        Er  ("VarDecl " ++ stringArtgType typ ++ " wrong type assigned")
                              | otherwise = addMessage ("VarDecl " ++ stringArtgType typ) exprt
      where
        exprt = typeExpr ex db

typeCheck db (GlobalVarDecl typ ex) | typeArgtype typ == (getType$ typeExpr ex db) = Ok
                              | otherwise = Er  ("GlobalVarDecl " ++ stringArtgType typ ++ " wrong type assigned")

typeCheck db fun@(FunCall name exprs)
                              | boolTypeError (traceShowId nomer) == False = nomer
                              | not$ boolTypeError exprt = exprt
                              | findinDb name db == Crt SimplyNull = Ok
                              | otherwise =  Er "Non void methods called "
      where
        exprt = checkCorrectFuncCommand db fun
        param = findMethodParamsDB db name
        nomer | fst param = checkRefParam (snd param) exprs
              | otherwise = Er "No params because didnt find method "

typeCheck db (Ass name ex) | findinDb name db == res = Ok
                           | boolTypeError res = Er ("Wrong type in Assigmnet "++name)
                           | otherwise = addMessage ("Assigment "++ name) res
      where
        res = typeExpr ex db
typeCheck db (Decr name) | findinDb name db == Crt SimplyInt = Ok
                              | otherwise = Er (name++" is not int, cannot --")

typeCheck db (Incr name) | findinDb name db == Crt SimplyInt = Ok
                              | otherwise = Er (name++" is not int, cannot ++")

typeCheck db (AddCom name ex) | findinDb name db == exprt = Ok
                              | not (boolTypeError exprt) = addMessage ("Add Command "++ name) exprt
                              | otherwise = Er ("Type error Add Command "++ name)
      where
        exprt = typeExpr ex db

typeCheck db (MinCom name ex) | findinDb name db == exprt = Ok
                              | not (boolTypeError exprt) = addMessage ("Min Command "++ name) exprt
                              | otherwise = Er ("Type error in Min Command "++ name)
      where
        exprt = typeExpr ex db

typeCheck db (While cond _) | boolTypeError$ condCheck = Ok
                            | otherwise = addMessage "While" condCheck
      where
        condCheck = typeCheckCondition cond db
typeCheck db (IfCom cond bloc1 bloc2) | boolTypeError$ condCheck = Ok
                            | not$  ret1 == Return NullExpr = Er "First block of IfCom has return"
                            | not$  ret2 == Return NullExpr = Er "Second block of IfCom has return"
                              | otherwise = addMessage "IfCom" condCheck
      where
        condCheck = typeCheckCondition cond db
        ret1 = findRetinBloc$ fromBlock bloc1
        ret2 = findRetinBloc$ fromBlock bloc2
typeCheck _ _ = Ok








-- this methdos takes and expression and the database and returns the expression's type
-- used in -> typeCheck, exprTypeFromRet, typeCheckCondition
-- if the exprssions withing the argument expresson are wrong, an appropriete error message will be retrned
typeExpr :: Expr -> [DataBase]-> TypeError
typeExpr (Constant _ ) db =Crt SimplyInt
typeExpr (BoolConst _ ) db = Crt SimplyBol
typeExpr (Mult e1 e2 ) db | not (t1 == t2) = Er "Multiplication elements are not the same type"
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

typeExpr (IfExpr typ cond e1 e2 ) db | cmp2 && cmp3 && conCorrectness = Crt  typ
                                     | cmp2 && cmp3 = addMessage "IfExpr" condcomp
                                     | cmp2 && conCorrectness = Er "Second expr of IfExpr is wrong type"
                                     | cmp3 && conCorrectness = Er "First expr of IfExpr is wrong type"
                                     | cmp2  = addMessage "IfExpr and second expr is wrong type" condcomp
                                     | cmp3  = addMessage "IfExpr and first expr is wrong type" condcomp
                                     | otherwise = Er "Both exprs in IfExpr are wrong"
        where
          t1 =getType $  typeExpr e1 db
          t2 = getType $ typeExpr e2 db
          cmp2 = t1 == typ
          cmp3 = t2 == typ
          condcomp = typeCheckCondition cond db
          conCorrectness = boolTypeError condcomp

typeExpr (Identifier x ) db = findinDb x db
typeExpr fun@(Funct name exprs) db | checkCorrectFuncExpr db fun =  findinDb name db
                                   | otherwise = Er "Function's arguments are not correct in FuncExpr"

typeExpr _ _ = Crt  SimplyNull


--this methods looks in the database for a namse and returns its type
-- used in determining a exprssion's type -> typeExpr
findinDb:: String -> [DataBase] -> TypeError
findinDb name ((DB  arg _ _):dbx) | name == stringArtgType arg = Crt $ typeArgtype arg
                                  | otherwise = findinDb name dbx
findinDb name ((DBF  arg  _ _):dbx) | name == stringArtgType arg = Crt$ typeArgtype arg
                                  | otherwise = findinDb name dbx
findinDb name ((Err arg er):dbx)  |  name == stringArtgType arg = Er "Errounous var called"
                                  | otherwise = findinDb name dbx
findinDb _ [] = Er "Undeclared variable called"


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


findMethodParamsDB :: [DataBase] -> String ->(Bool,[Param])
findMethodParamsDB ((DBF fname params _):db) name | stringArtgType fname == name = (True,params)
                                                | otherwise = findMethodParamsDB db name
findMethodParamsDB ((DB _ _ _):db) name = findMethodParamsDB db name
findMethodParamsDB ((Err arg er):db) name |  stringArtgType arg == name = (False, [])
                                          | otherwise = findMethodParamsDB db name
findMethodParamsDB [] _ = (False,[])

-- compares if a list of parameter and a list of expression is the same type
comparaParamsandArgs :: [DataBase] -> [Param] -> [Expr] -> Bool
comparaParamsandArgs _ [] [] = True
comparaParamsandArgs _ [] _ = False
comparaParamsandArgs _ _ [] = False
comparaParamsandArgs db (para:params) (e:exprs) | getParamType para == (getType$typeExpr e db)
                                        = comparaParamsandArgs db params exprs
                          | otherwise = False


checkCorrectFuncExpr :: [DataBase] -> Expr -> Bool
checkCorrectFuncExpr db (Funct name args ) | fst param = comparaParamsandArgs db (snd param) args
                                           | otherwise = False
  where
    param = findMethodParamsDB db name

checkCorrectFuncCommand :: [DataBase] -> Commands-> TypeError
checkCorrectFuncCommand db (FunCall name args ) | fst  param == False = Er "Could not find Method in DB"
                                           | not $(boolTypeError byRefCorr) = byRefCorr
                                           | comparaParamsandArgs db (snd param) args = Ok
                                           | otherwise = Er "Function's arguments are not correct"
  where
    param = findMethodParamsDB db name
    byRefCorr =checkRefParam (snd param) args




--methods that filters out all the NON-erros from the Symbol Table
showErrorinDB :: [DataBase] -> [DataBase]
showErrorinDB [] = []
showErrorinDB ((Err ar x):db) =(Err ar x) : showErrorinDB db
showErrorinDB (x:db)= showErrorinDB db

--adds an array of TypeErros into a symbol Table
-- method used to put together all the erros of a program
addTypeErrstoDB:: [TypeError] -> [DataBase] -> [DataBase]
addTypeErrstoDB [] db = db
addTypeErrstoDB (x:xs) db =Err (Arg SimplyNull "") x :addTypeErrstoDB xs db













--eof
