module Generator where

import Sprockell
import Parser
import TreeWalker
import Text.Parsec.String
import Data.Maybe

--Block [VarDecl (Arg SimplyInt "jesse") (Constant 1000),
-- GlobalVarDecl (Arg SimplyInt "robert") (Constant 1000),
-- VarDecl (Arg SimplyInt "marieke") (Constant 5000),FunDecl (Arg SimplyInt "transfer") [ByRef (Arg SimplyInt "from"),ByRef (Arg SimplyInt "to"),ByVal (Arg SimplyInt "amount")] (Block [IfCom (Gq (Identifier "from") (Identifier "amount")) (Block [MinCom "from" (Identifier "amount"),AddCom "to" (Identifier "amount"),End]) (Block [End]),End]),FunDecl (Arg SimplyNull "helicopterMoney") [ByVal (Arg SimplyInt "to"),ByVal (Arg SimplyInt "amount")] (Block [AddCom "to" (Identifier "amount"),While (Lt (Identifier "to") (Identifier "robert")) (Block [AddCom "to" (Identifier "amount"),End]),End]),Fork (Block [FunCall "helicopterMoney" [Identifier "jesse",Constant 9000],End]

generation :: Bloc -> [Instruction]
generation xs = genBlock commands smTable ++ [EndProg]
  where
    commands = fromBlock xs
    smTable = treeBuilder commands 1 []

-- generationHelper :: [Commands] -> [DataBase] -> [Instruction]
-- -- generationHelper [] _ =
-- generationHelper xs smTable |length xs > 1 = genBlock xs smTable
--                             |otherwise = [EndProg]

genBlock :: [Commands] -> [DataBase] -> [Instruction]
genBlock [] _ = []
genBlock (x:xs) smTable = gen x smTable ++ genBlock xs smTable


gen :: Commands -> [DataBase] -> [Instruction]
-- Generate end
gen End _ = []
gen Parser.Nop _ = [Sprockell.Nop]
-- Generate a variable
gen (VarDecl (Arg varType name) expr) smTable = genVar name expr smTable -- genExpr expr smTable ++ -- pop that expression into regD
--                                    memAddr smTable name ++ -- eddress is in regE
--                                    [Pop regD, Store regD (IndAddr regE)]
                                   -- [Load (IndAddr regE) regD, -- pop anythingg left in regD
                                   -- Push regD] -- store val in regE in regD
                                   -- Generate an (re-)assignment of a variable
-- Generate a (re)assignment of a variable it's same as above
gen (Ass varName expr) smTable = genVar varName expr smTable

-- Generate a print method
gen (Print expr) smTable = genExpr expr smTable ++
                           [Pop regE, WriteInstr regE numberIO]

-- Generate if condition
gen (IfCom cond thenBlock elseBlock) smTable
  = genCond cond smTable
  ++ [Pop regC,
      ComputeI Xor regC 1 regC, -- Branch is implemented stupidly so we need to xor (negate) it
      Branch regC (Rel (length genThen + 2)) -- plus two to also skip the branch over the else statement
  ] ++ genThen ++ [Jump (Rel (length genElse + 1))] ++ genElse
  where
    genThen = genBlock (fromBlock thenBlock) smTable
    genElse = genBlock (fromBlock elseBlock) smTable

-- generate while statement and block
gen (While cond block) smTable
  = genWhileCond
  ++ [Pop regC,
      ComputeI Xor regC 1 regC, -- do this comparison again
      Branch regC (Rel (lengthWhileBody + 2))] -- If nope, skip the entire while body
  ++ genWhileBody ++ [Jump (Rel (-(length genWhileCond + lengthWhileBody + 3)))] -- then go back to the comparison

  where
    genWhileCond = genCond cond smTable
    genWhileBody = genBlock (fromBlock block) smTable
    lengthWhileBody = length genWhileBody

-- Generate code for decreasing a value
gen (Parser.Decr varName) smTable
  = memAddr smTable varName -- eddress is in regE
  ++ [Load (IndAddr regE) regD -- store the actual value in regD
      , Compute Sprockell.Decr regD regD regD
      , Store regD (IndAddr regE)] -- store back in regE again

-- Generate code for increasing a value
gen (Parser.Incr varName) smTable
  = memAddr smTable varName -- eddress is in regE
  ++ [Load (IndAddr regE) regD -- store the actual value in regD
      , Compute Sprockell.Incr regD regD regD
      , Store regD (IndAddr regE)] -- store back in regE again
-- Generate code for += statements
gen (AddCom varName expr) smTable =
  genExpr expr smTable -- pop the result in regD
  ++ memAddr smTable varName -- eddress is in regE
  ++ [Pop regD -- pop the genExpr in regD
      , Load (IndAddr regE) regA -- load the value of the address in regA
      , Compute Sprockell.Add regA regD regA -- add that stuff to regA
      , Store regA (IndAddr regE)] -- store regA back in regE

-- Same as before, Generate code for -= statements
gen (MinCom varName expr) smTable =
  genExpr expr smTable -- pop the result in regD
  ++ memAddr smTable varName -- eddress is in regE
  ++ [Pop regD -- pop the genExpr in regD
      , Load (IndAddr regE) regA -- load the value of the address in regA
      , Compute Sub regA regD regA -- add that stuff to regA
      , Store regA (IndAddr regE)] -- store regA back in regE



genVar :: String -> Expr -> [DataBase] -> [Instruction]
genVar name expr smTable = genExpr expr smTable  -- Pop that into regD
                           ++ memAddr smTable name -- edress is in regE
                           ++ [Pop regD, Store regD (IndAddr regE)]

genExpr :: Expr -> [DataBase] -> [Instruction]
-- Generate constant
genExpr (Constant i) smTable = [Load (ImmValue (fromInteger i)) regE, Push regE]
-- Generate a boolean
genExpr (BoolConst bool) smTable = [Load (ImmValue (boolToInt bool)) regE, Push regE]
-- Generate the parens, just evaluate the expr in between it
genExpr (Paren expr) smTable = genExpr expr smTable
-- Generate a reference
genExpr (Identifier name) smTable = memAddr smTable name
                                    ++ [Load (IndAddr regE) regD, Push regD]
-- Generate a calculation of two expressions
genExpr (Parser.Mult exp1 exp2) smTable = genTwoExpr (Sprockell.Mul, exp1, exp2) smTable
genExpr (Parser.Add exp1 exp2) smTable  = genTwoExpr (Sprockell.Add, exp1, exp2) smTable
genExpr (Parser.Min exp1 exp2) smTable  = genTwoExpr (Sprockell.Sub, exp1, exp2) smTable

-- Else error out
genExpr _ smTable = error "genExpr error"


-- Generate the code for doing a condition with two expressions
genCond :: Condition -> [DataBase] -> [Instruction]
genCond (Parser.Lt exp1 exp2) smTable = genTwoExpr (Sprockell.Lt,    exp1, exp2) smTable
genCond (Parser.Eq exp1 exp2) smTable = genTwoExpr (Sprockell.Equal, exp1, exp2) smTable
genCond (Parser.Gt exp1 exp2) smTable = genTwoExpr (Sprockell.Gt,    exp1, exp2) smTable
genCond (Parser.Lq exp1 exp2) smTable = genTwoExpr (Sprockell.LtE,   exp1, exp2) smTable
genCond (Parser.Gq exp1 exp2) smTable = genTwoExpr (Sprockell.GtE,   exp1, exp2) smTable

-- Generate a calculation of two expressions. Evaluates both expressions and pushes back the result to the stack.
genTwoExpr :: (Operator, Expr, Expr) -> [DataBase] -> [Instruction]
genTwoExpr (op, exp1, exp2) smTable = genExpr exp1 smTable ++ genExpr exp2 smTable
                                      ++ [Pop regB, Pop regA,
                                         Compute op regA regB regA,
                                         Push regA]

-- Helper function to convert an boolean to an int
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0


-- genCond cond smTable = genExpr exp1 smTable ++ genExpr exp2 smTable
--                                  ++ [Pop regB, Pop regA,
--                                  Compute op regA regB regA,
--                                  Push regA]
--                                  where
--                                    (op, exp1, exp2) = getCond cond
-- -- Helper function to get conver our Condition to something Sprockell can understand
-- getCond :: Condition -> (Sprockell.Operator, Expr, Expr)
-- getCond (Parser.Lt exp1 exp2) = (Sprockell.Lt,    exp1, exp2)
-- getCond (Parser.Eq exp1 exp2) = (Sprockell.Equal, exp1, exp2)
-- getCond (Parser.Gt exp1 exp2) = (Sprockell.Gt,    exp1, exp2)
-- getCond (Parser.Lq exp1 exp2) = (Sprockell.LtE,   exp1, exp2)
-- getCond (Parser.Gq exp1 exp2) = (Sprockell.GtE,   exp1, exp2)


-- Get the memory address
memAddr :: [DataBase] -> String -> [Instruction]
memAddr smTable varName = [Compute Sprockell.Add regF reg0 regE]
                  ++ replicate x (Load (IndAddr regE) regE)
                  ++ [ComputeI Sprockell.Add regE y regE]
                  where
                    (x', y') = getOffset smTable varName
                    x = x' - 1
                    y = y' + 1

--------- DEBUG REMOVE WHEN DONE!!
codeGenTest = do
  result <- parseFromFile parseBlock "../examples/whiletest.amv"
  case result of
    Left err -> print err
    Right xs -> do
      print code
      run [code]
      where
        code = generation xs

-- koekje = [[Load (ImmValue 9999) 6,
-- Push 6,
-- Compute Add 7 0 6,
-- ComputeI Add 6 1 6,
-- Pop 5,
-- Store 5 (IndAddr 6),
-- Compute Add 7 0 6,
-- ComputeI Add 6 1 6,Load (IndAddr 6) 5,Push 5,Pop 6,WriteInstr 6 (DirAddr 65536), -- print marieke 1
-- Compute Add 7 0 6,ComputeI Add 6 1 6,Load (IndAddr 6) 5,Push 5,Load (ImmValue 9000) 6,Push 6,Pop 3,Pop 2,Compute Lt 2 3 2,
-- Push 2,Pop 4,Branch 4 (Rel 6), -- 6 VERY IMPORTANT
-- Compute Add 7 0 6, -- 1
-- ComputeI Add 6 1 6, -- 2
-- Load (IndAddr 6) 5, -- 3
-- Push 5, -- 4
-- Pop 6, --5
-- WriteInstr 6 (DirAddr 65536), --6
-- Compute Add 7 0 6,ComputeI Add 6 1 6,Load (IndAddr 6) 5,Push 5,Pop 6,WriteInstr 6 (DirAddr 65536),EndProg]]

  -- [Load (ImmValue 1000) 6,
  -- Push 6,
  -- Compute Add 7 0 6,
  -- ComputeI Add 6 1 6,
  -- Load (IndAddr 6) 5,
  -- Push 5,
  -- Compute Add 7 0 6,
  -- ComputeI Add 6 1 6,
  -- Pop 5,
  -- Store 5 (IndAddr 6),
  -- Pop 6,
  -- WriteInstr 6 (DirAddr 65536),
  -- EndProg]


-- prog :: [Instruction]
-- prog = [ Load (ImmValue 1000) regA,
--          Store regA (DirAddr  0),         -- int jesse = 1000 (@0)
--          Load (ImmValue 1001) regA,
--          Store regA (DirAddr  1),         -- int robert = 1000 (@1)
--
--          Load (DirAddr 0) regA,
--          WriteInstr regA numberIO,
--
--          Load (DirAddr 1) regA,
--          WriteInstr regA numberIO,
--
--          -- Jump ahead to after the function(s)
--
--          -- transfer (jesse, marieke, 100)
--          Sprockell.Nop,  -- transfer
--          Pop regB, --(regA will be return address)
--          Pop regC, -- the from parameter DirAddr   (call by ref)
--          Pop regD, -- the to parameter  DirAddr    (call by ref)
--          Pop regE, -- the amount parameter just imm value
--
--          Load (IndAddr regC) regA, -- FROM: get jesse
--          -- if from >= amount
--          Compute GtE regA regE regA,
--          Branch regA (Rel 1), -- JUMP TO ELSE NOT TO REL 1
--
--          Load (IndAddr regC) regA, -- FROM: get jesse
--          Compute Sub regA regE regA,
--          Store regA (IndAddr regC),
--
--          Load (IndAddr regD) regA,
--          Compute Sprockell.Add regA regE regA,
--          Store regA (IndAddr regD),
--
--
--
--
--
--
--          -- so when I see a function then store my address into
--
--
--
--        -- -- "beginloop"
--        -- , Compute Gt regA regE regC     -- regA > regE ?
--        -- , Branch regC (Abs 13)          -- then jump to target "end"
--        -- , WriteInstr regA numberIO      -- output regA
--        -- , Compute Add regA regB regA
--        -- , Compute Gt regB regE regC     -- regB > regE
--        -- , Branch regC (Abs 13)          -- target "end"
--        -- , WriteInstr regB numberIO      -- output regB
--        -- , Compute Add regA regB regB
--        -- , Jump (Rel (-8))               -- target "beginloop"
--
--        -- "end"
--         EndProg
--        ]
--
-- -- run the prog on 1 Sprockell core
-- cheese = run [prog]
