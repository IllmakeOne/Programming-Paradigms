

import Parser
import TreeWalker
import BasicParsers
import Structure
import Text.ParserCombinators.Parsec
-- import Data
import TreeWalkerTests


data ParseTest =  ParseT { testName :: String, testParse :: Bool }


pt = [
      ------------------------------------------------------------------------
-------------------------------Expression parsing Tests-------------------------------------
      ------------------------------------------------------------------------
    ParseT {
        testName = "parseCondition_test1",
        testParse = parseCondition_test1
        },
    ParseT {
        testName = "parseCondition_test3",
        testParse = parseCondition_test3
        },
    ParseT {
        testName = "parseTerm_test2",
        testParse =  parseTerm_test2
        },
    ParseT {
        testName = "arguments_test1",
        testParse =  arguments_test1
        },
    ParseT {
        testName = "parseExpr_testnu",
        testParse =  parseExpr_testnu
        },
    ParseT {
        testName = "parseExpr_test1",
        testParse =  parseExpr_test1
        },
    ParseT {
        testName = "parseIfexpr_tesst1",
        testParse =  parseIfexpr_tesst1
        },
    ParseT {
        testName = "parseIfexpr_tesst2",
        testParse =  parseIfexpr_tesst2
        },
    ParseT {
        testName = "parsefactor_testFunct",
        testParse =  parsefactor_testFunct
        },
    ParseT {
        testName = "parsefactor_testIfexpr",
        testParse =  parsefactor_testIfexpr
        },


    ------------------------------------------------------------------------
-------------------------------Command parsing Tests-------------------------------------
    ------------------------------------------------------------------------
    ParseT {
        testName = "parseCommand_testJoin",
        testParse =  parseCommand_testJoin
        },
    ParseT {
        testName = "parsefactor_testIfexpr",
        testParse =  parsefactor_testIfexpr
        },
    ParseT {
        testName = "parseBlock_test1",
        testParse =  parseBlock_test1
        },
    ParseT {
        testName = "parseIfCom_tesst2",
        testParse = parseIfCom_tesst2
        },
    ParseT {
        testName = "parseFunDecl_test2",
        testParse = parseFunDecl_test2
        },
    ParseT {
        testName = "parseIncr_test1",
        testParse = parseIncr_test1
        },
    ParseT {
        testName = "params_test1",
        testParse = params_test1
        },


    ------------------------------------------------------------------------
-------------------------------Elaboration Tests-------------------------------------
    ------------------------------------------------------------------------

    ParseT {
        testName = "scopesTracker_test1",
        testParse = scopesTracker_test1
        },
    ParseT {
        testName = "scopesTracker_test2",
        testParse = scopesTracker_test2
        },
    ParseT {
        testName = "increaseOffset_test1",
        testParse = increaseOffset_test1
        },
    ParseT {
        testName = "increaseOffset_test2",
        testParse = increaseOffset_test2
        },
    ParseT {
        testName = "typeCheckProgram_test3",
        testParse = typeCheckProgram_test3
        },

    ------------------------------------------------------------------------
-------------------------------TypeChcking Tests-------------------------------------
    ------------------------------------------------------------------------
    ParseT {
        testName = "typeCheck_test_vadecl1",
        testParse = typeCheck_test_vadecl1
        },
    ParseT {
        testName = "typeCheck_test_globalvadecl2",
        testParse = typeCheck_test_globalvadecl2
        },
    ParseT {
        testName = "typeCheckProgram_test1",
        testParse = typeCheckProgram_test1
        },
    ParseT {
        testName = "typeCheck_test_addcom2",
        testParse = typeCheck_test_addcom2
        },
    ParseT {
        testName = "typeCheck_test_funcall2",
        testParse = typeCheck_test_funcall2
        },
    ParseT {
        testName = "typeCheck_test_globalvadecl2",
        testParse = typeCheck_test_globalvadecl2
        },
    ParseT {
        testName = "typeCheckProgram_test1",
        testParse = typeCheckProgram_test1
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
