module Tests where


import Parser
import TreeWalker
import BasicParsers
import Structure
import Text.ParserCombinators.Parsec
-- import Data


data ParseTest =  ParseT { testName :: String, testParse :: Bool }

-- name :: ParseTest -> String

-- problems = [ParseTest]
pt = [
      ------------------------------------------------------------------------
-------------------------------Expression parsing Tests-------------------------------------
      ------------------------------------------------------------------------
    ParseT {
        testName = "parseCondition_test1",
        testParse = parse parseCondition "" "2 == fib(2)" ==
              (Right (Eq (Constant 2) (Funct "fib" [Constant 2])))
        },
    ParseT {
        testName = "parseCondition_test3",
        testParse = parse parseCondition "" " 2 >= fib(2)" ==
              (Right (Gq (Constant 2) (Funct "fib" [Constant 2])))
        },
    ParseT {
        testName = "parseTerm_test2",
        testParse =  parse parseExpr "" "2*4*4" ==
              (Right (Mult (Constant 2) (Mult (Constant 4) (Constant 4))))
        },
    ParseT {
        testName = "arguments_test1",
        testParse =  parse arguments "" "2,x,5" ==
              (Right [Constant 2,Identifier "x",Constant 5])
        },
    ParseT {
        testName = "parseExpr_testnu",
        testParse =  parse parseExpr "" "nu" ==
              (Right (BoolConst False))
        },
    ParseT {
        testName = "parseExpr_test1",
        testParse =  parse parseExpr "" "3+3*2" ==
              (Right (Add (Constant 3) (Mult (Constant 3) (Constant 2))))
        },
    ParseT {
        testName = "parseIfexpr_tesst1",
        testParse =  parse parseExpr "" "int ?(2==2){2+1}{2}" ==
              (Right (IfExpr SimplyInt (Eq (Constant 2) (Constant 2)) (Add (Constant 2) (Constant 1)) (Constant 2)))
        },
    ParseT {
        testName = "parseIfexpr_tesst1",
        testParse =  parse parseExpr "" "bool ?(2==2){nu}{ya}" ==
              (Right (IfExpr SimplyBol (Eq (Constant 2) (Constant 2)) (BoolConst False) (BoolConst True)))
        },
    ParseT {
        testName = "parsefactor_testFunct",
        testParse =  parse parseExpr "" "fib(2,3,ya)" ==
              (Right (Funct "fib" [Constant 2,Constant 3,BoolConst True]))
        },
    ParseT {
        testName = "parsefactor_testIfexpr",
        testParse =  parse parseExpr "" "int ?(2<x){2+x}{2+3}" ==
              (Right (IfExpr SimplyInt (Lt (Constant 2) (Identifier "x")) (Add (Constant 2) (Identifier "x")) (Add (Constant 2) (Constant 3))))
        },


    ------------------------------------------------------------------------
-------------------------------Command parsing Tests-------------------------------------
    ------------------------------------------------------------------------
    ParseT {
        testName = "parseCommand_testJoin",
        testParse =  parse parseCommand "" "join ;" == Right Join
        },
    ParseT {
        testName = "parsefactor_testIfexpr",
        testParse =  parse parseCommand "" "return int ?(x<2) {2}{3};" ==
              (Right (Return (IfExpr SimplyInt (Lt (Identifier "x") (Constant 2)) (Constant 2) (Constant 3))))
        },
    ParseT {
        testName = "parseBlock_test1",
        testParse =  parse parseBlock "" "{ nop;nop;}" ==
              (Right (Block [Nop,Nop,End]))
        },
    ParseT {
        testName = "parseIfCom_tesst2",
        testParse = parse parseCommand "" "if (from >= amount) { from -= amount; to += amount;} {};" ==
              (Right (IfCom (Gq (Identifier "from") (Identifier "amount")) (Block [MinCom "from" (Identifier "amount"),AddCom "to" (Identifier "amount"),End]) (Block [End])))
        },
    ParseT {
        testName = "parseFunDecl_test2",
        testParse = parse parseCommand "" "func void theStuff(int a, bool b){ int x = a;}" ==
              (Right (FunDecl (Arg SimplyNull "theStuff") [ByVal (Arg SimplyInt "a"),ByVal (Arg SimplyBol "b")] (Block [VarDecl (Arg SimplyInt "x") (Identifier "a"),End])))
        },
    ParseT {
        testName = "parseIncr_test1",
        testParse = parse parseCommand "" "x ++;" ==
              (Right (Incr "x"))
        },
    ParseT {
        testName = "params_test1",
        testParse = parse params "" "& bool h, \n int x, \n bool x" ==
              (Right [ByRef (Arg SimplyBol "h"),ByVal (Arg SimplyInt "x"),ByVal (Arg SimplyBol "x")])
        },


    ------------------------------------------------------------------------
-------------------------------Elaboration Tests-------------------------------------
    ------------------------------------------------------------------------

    ParseT {
        testName = "scopesTracker_test1",
        testParse = scopesTracker [(1,0),(2,4)] 2 == 4
        },
    ParseT {
        testName = "scopesTracker_test2",
        testParse = scopesTracker [(1,0),(2,4)] 1 == 0
        },
    ParseT {
        testName = "increaseOffset_test2",
        testParse = increaseOffset [] 2 == [(2,0)]
        },
    ParseT {
        testName = "increaseOffset_test2",
        testParse = increaseOffset [(1,0),(2,4)] 2 == [(1,0),(2,5)]
        },
    ParseT {
        testName = "treeBuilder_test1",
        testParse = treeBuilder_test1 == treeBuilder_test1_correct
        },
    ParseT {
        testName = "treeBuilder_test2",
        testParse = treeBuilder_test2 == treeBuilder_test2_correct
        },

    ------------------------------------------------------------------------
-------------------------------TypeChcking Tests-------------------------------------
    ------------------------------------------------------------------------
    ParseT {
        testName = "typeCheck_test_vadecl1",
        testParse = typeCheck_test_vadecl1 == [Er "VarDecl x wrong type assigned"]
        },
    ParseT {
        testName = "typeCheck_test_globalvadecl2",
        testParse = typeCheck_test_globalvadecl2 == [Er "GlobalVarDecl x wrong type assigned"]
        }
    ]


runTests :: [ParseTest] -> Bool
runTests (x:xs) | testParse x = runTests xs
                -- | otherwise = ( print (testName x)  >> return False)
                | otherwise = error ((testName x)++" failed")
runTests [] = True


main | runTests pt = print "All tests which should pass, passed"
     | otherwise = error " Something very wrong in main test"



-- ughguh = runTests pt `catchError` print "sadasdas"
















treeBuilder_test1_correct = [DB (Arg SimplyInt "z") 0 0,DBF (Arg SimplyInt "fib") [ByVal (Arg SimplyInt "x"),ByRef (Arg SimplyInt "y")] (Block [VarDecl (Arg SimplyInt "x") (Constant 2),Return (Identifier "y"),End]),DB (Arg SimplyInt "x") 2 1,DB (Arg SimplyInt "x") 1 0,DBF (Arg SimplyInt "fibi") [ByVal (Arg SimplyInt "x"),ByRef (Arg SimplyInt "y")] (Block [VarDecl (Arg SimplyInt "x") (Constant 2),Return (Identifier "y"),End]),DB (Arg SimplyInt "x") 2 1]
treeBuilder_test2_correct = [DB (Arg SimplyInt "a") 0 0,DB (Arg SimplyBol "b") 0 1,DB (Arg SimplyInt "c") 1 0,DB (Arg SimplyBol "d") 1 1,DB (Arg SimplyBol "e") 0 2,DBF (Arg SimplyNull "aux") [] (Block [VarDecl (Arg SimplyInt "b") (Constant 0),End]),DB (Arg SimplyInt "b") 2 1]
