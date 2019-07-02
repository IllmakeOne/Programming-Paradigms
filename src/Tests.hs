module Tests where


import Parser
import TreeWalker
import BasicParsers


type ParseTest =  { testName :: String, parseTest :: Bool}


-- problems = [ParseTest]
problems = [
    ParseT {
        problemName = "parseCondition_test1",
        evalProblem =
              Right (Add (Constant 3) (Mult (Constant 3) (Constant 2))) == parse parseCondition "" "2 < fib(2)"
        },
    ParseT {
        problemName = "parseCondition_test2",
        evalProblem =  parse parseCondition "" "2 < fib(2)" ==  Right (Lt (Constant 2) (Funct "fib" [Constant 2]))
        }
    ]
