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
-------------Expssion Tests-------------------------
    ParseT {
        testName = "parseCondition_test1",
        testParse = parse parseCondition "" "2 == fib(2)" ==
              (Right (Eq (Constant 2) (Funct "fib" [Constant 2])))
        },
    ParseT {
        testName = "parseCondition_test2",
        testParse =  parse parseCondition "" "2 < fib(2)" ==
              (Right (Lt (Constant 2) (Funct "fib" [Constant 2])))
        },
    ParseT {
        testName = "parseCondition_test3",
        testParse = parse parseCondition "" " 2 >= fib(2)" ==
              (Right (Gq (Constant 2) (Funct "fib" [Constant 2])))
        },
    ParseT {
        testName = "parseTerm_test2",
        testParse =  parse parseTerm "" "2*4*4" ==
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
        testParse =  parse parseFactor "" "fib(2,3,ya)" ==
              (Right (Funct "fib" [Constant 2,Constant 3,BoolConst True]))
        },
    ParseT {
        testName = "parsefactor_testIfexpr",
        testParse =  parse parseFactor "" "int ?(2<x){2+x}{2+3}" ==
              (Right (IfExpr SimplyInt (Lt (Constant 2) (Identifier "x")) (Add (Constant 2) (Identifier "x")) (Add (Constant 2) (Constant 3))))
        },
-------------------Command Tests-------------------------------------
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
        testName = "parseVarDecl_testbool2",
        testParse = parse parseVarDecl "" "bool x = ya;" ==
              (Right (VarDecl (Arg SimplyBol "x") (BoolConst True)))
        }

    ]


runTests :: [ParseTest] -> Bool
runTests (x:xs) | testParse x = runTests xs
                -- | otherwise = ( print (testName x)  >> return False)
                | otherwise = error ((testName x)++" failed")
runTests [] = True


main | runTests pt = print "All tests which should pass, passed"
     | otherwise = error " Something very wrong in main test"
