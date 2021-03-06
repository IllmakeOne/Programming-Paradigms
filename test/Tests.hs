import Parser
import ParseExpr
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
        testName = "parseExpr_test3",
        testParse =  parseExpr_test3
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
                | otherwise = error ("Tests: " ++ testName x ++" failed!")
runTests [] = True

main :: IO ()
main = do
  let success = runTests pt
  if success then putStrLn "All frontend tests which should pass, passed"
             else error " Something went wrong in main test"
