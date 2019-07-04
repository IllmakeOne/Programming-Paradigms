module Generator where

import Sprockell
import Parser
import TreeWalker
import Structure
import Text.Parsec.String
import Data.Maybe
import Debug.Trace
import Data.List

generation :: Bloc -> [Instruction]
generation xs = genBlock commands smTable ++ [EndProg]
  where
    commands = fromBlock xs
    smTable = treeBuilder commands 1 []

-- generationHelper :: [Commands] -> [DataBase] -> [Instruction]
-- -- generationHelper [] _ =
-- generationHelper xs smTable |length xs > 1 = genBlock xs smTable
--                             |otherwise = [EndProg]

-- Generate a new block. First it generates the inner functions, so you can call
--  them earlier, before they are actually defined in the AST. The first thing
--  will determine if we need to jump over the declared functions or not, and then
--- generates the main body parts.
genBlock :: [Commands] -> [DataBase] -> [Instruction]
genBlock [] _ = []
-- Here span funcdecl commands
genBlock commands smTable = jumpOverFuncs -- gen x smTable ++ genBlock xs smTable
                            ++ generatedFunctions
                            ++  genCommands body smTable ++ (traceShow fnTable) []
  where
    (functions, body) = partition isFunction commands
    (generatedFunctions, fnTable)  = genFuncs functions smTable (length jumpOverFuncs) []
    jumpOverFuncs = jumpOrNot (length generatedFunctions + length functions)


genFuncs :: [Commands] -> [DataBase] -> Int -> [(String, Int)] -> ([Instruction] , [(String, Int)])
genFuncs [] _ _ fnTable = ([], fnTable)
genFuncs (x:xs) smTable offs fnTable = (instructions ++ otherInstructions, finalFnTable)
  where
    (instructions, newFnTable)    = genFunc x smTable offs fnTable
    (otherInstructions, finalFnTable) = genFuncs xs smTable (offs + length instructions) newFnTable

genFunc :: Commands -> [DataBase] -> Int -> [(String, Int)] -> ([Instruction] , [(String, Int)])
genFunc (FunDecl (Arg ftype fname) params body) smTable startOffs fnTable=
  ([ Debug ("func: " ++ fname),
    Load (ImmValue (1 + 3 * length params)) regA,
    Compute Sub regF regA regA, -- 2. set the value of the ARP
    Load (IndAddr reg0) regD, -- 3. amount of parameters already passed down

    ComputeI Sprockell.Gt regD (length params ) regE, -- 4.  LOOP check if all parameters are passed
    Branch regE (Rel 7), -- 5. then skip this parameter loading below

    Load (IndAddr regA) regB, -- 6. Load the actual parameter value
    Compute Sprockell.Add regF regD regE, -- 7. Store the address in the local data
    Store regB (IndAddr regE), -- 8.
    Compute Sprockell.Incr regD regD regD, -- 9.
    ComputeI Sprockell.Add regA 3 regA, -- 10.

    Jump (Rel (-7))] -- 11. go back to  LOOP
    ++ genBlock (fromBlock body) smTable

    ++ [Load (ImmValue (3 * length params)) regA,
       Compute Sub regF regA regA,
       ComputeI Sprockell.Add reg0 1 regD,

       ComputeI Sprockell.Gt regD (length params) regE, -- LOOP
       Branch regE (Rel 23), -- then jump over this shit
       Compute Sprockell.Add regF regD regE,
       Load (IndAddr regE) regC,
       Load (IndAddr regA) regB,
       Compute Sprockell.Lt regB reg0 regE, -- check if it is correct
       Branch regE (Rel 2),
       Store regC (IndAddr regB), -- then save in address
       Compute Sprockell.Incr regA reg0 regA, -- move pointr to global
       Branch regE (Rel 10),

       Compute Sprockell.Add regB reg0 regE, -- lock address
       TestAndSet (IndAddr regE),
       Receive regE,
       Branch regE (Rel 2), -- go back if lock fails
       Jump (Rel (-4)),
       ComputeI Sprockell.Add regB 1 regB,

       WriteInstr regC (IndAddr regB),
       ComputeI Sub regB 1 regB, --Unlock
       Compute Sprockell.Incr  regD regD regD, --add one to D
       ComputeI Sprockell.Add regA 2 regA,
       Jump (Rel (-23)), -- Go back to the LOOP

       Compute Sprockell.Decr regF reg0 regA,
       Load (IndAddr regA) regE, -- get the return address
       Load (IndAddr regF) regF, -- restore arp
       Debug ("func: " ++ fname ++ " end! We're only going to jump!"),
       Jump (Ind regE) -- jump to da return address
       ], (fname, startOffs) : fnTable)

genFunc command smTable _ fnTable = (gen command smTable, fnTable)

updateSmTable :: [DataBase] -> String -> Int -> [DataBase]
updateSmTable [] _ _ = []
updateSmTable (DBF (Arg ftype dbfname) params initOffset : xs) fname offset |
  dbfname == fname = (DBF (Arg ftype dbfname) params offset) : updateSmTable xs fname offset
updateSmTable (other:xs) fname offset = other : updateSmTable xs fname offset

jumpOrNot :: Int -> [Instruction]
jumpOrNot 0 = []
jumpOrNot i = [Jump (Rel i)]

isFunction :: Commands -> Bool
isFunction FunDecl{} = True
isFunction _ = False

genCommands :: [Commands] -> [DataBase] -> [Instruction]
genCommands [] _ = []
genCommands (x:xs) smTable = gen x smTable ++ genCommands xs smTable

-- ALSO OPEN A NEW SCOPE!!!


gen :: Commands -> [DataBase] -> [Instruction]
-- Generate end
gen End _ = []
gen Structure.Nop _ = [Sprockell.Nop]
-- Generate a variable
gen (VarDecl (Arg varType name) expr) smTable = genVar name expr Nothing smTable -- genExpr expr smTable ++ -- pop that expression into regD
--                                    memAddr smTable name ++ -- eddress is in regE
--                                    [Pop regD, Store regD (IndAddr regE)]
                                   -- [Load (IndAddr regE) regD, -- pop anythingg left in regD
                                   -- Push regD] -- store val in regE in regD
                                   -- Generate an (re-)assignment of a variable
-- Generate a (re)assignment of a variable it's same as above
gen (Ass varName expr) smTable = genVar varName expr Nothing smTable

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
gen (Structure.Decr varName) smTable = incrDecrVar varName Sprockell.Decr smTable
-- Generate code for increasing a value
gen (Structure.Incr varName) smTable = incrDecrVar varName Sprockell.Incr smTable
-- Generate code for += statements
gen (AddCom varName expr) smTable = genVar varName expr (Just Sprockell.Add) smTable
-- Same as before, Generate code for -= statements
gen (MinCom varName expr) smTable = genVar varName expr (Just Sprockell.Sub) smTable

-- Generate a new function
gen (FunDecl (Arg ftype fname) params body) smTable = error "funDecl should not be defined in the gen method"

-- Generate a value with an expression, and maybe an additional calculation
genVar :: String -> Expr -> Maybe Operator -> [DataBase] -> [Instruction]
genVar name expr op smTable = genExpr expr smTable  -- Pop that into regD
                           ++ memAddr smTable name -- edress is in regE
                           ++ [Pop regD]
                           ++ genVarCompute op
                           ++ [Store regD (IndAddr regE)]

-- Do we want to compute something with the variable? This helper function will
-- find out.
genVarCompute :: Maybe Operator -> [Instruction]
genVarCompute Nothing = []
genVarCompute (Just op) = [Load (IndAddr regE) regA, -- load the value of the address in regA
                           Compute op regA regD regD] -- Store the evaluation in regD

-- Function to just get the value from memory and increase/ decrease based on
-- the operator. The caller of this function has to make sure to only use Incr/ Decr.
incrDecrVar :: String -> Operator -> [DataBase] -> [Instruction]
incrDecrVar varName op smTable = memAddr smTable varName -- eddress is in regE
                                 ++ [Load (IndAddr regE) regD -- store the actual value in regD
                                    , Compute op regD regD regD
                                    , Store regD (IndAddr regE)] -- store back in regE again

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
genExpr (Structure.Mult exp1 exp2) smTable = genTwoExpr (Sprockell.Mul, exp1, exp2) smTable
genExpr (Structure.Add exp1 exp2) smTable  = genTwoExpr (Sprockell.Add, exp1, exp2) smTable
genExpr (Structure.Min exp1 exp2) smTable  = genTwoExpr (Sprockell.Sub, exp1, exp2) smTable

-- Generate an inline if statement
-- It's the same as if statement above but now with expressions instead of blocks.
genExpr (IfExpr _ cond exp1 exp2) smTable =
  genCond cond smTable
  ++ [Pop regC,
      ComputeI Xor regC 1 regC, -- Branch is implemented stupidly so we need to xor (negate) it
      Branch regC (Rel (length genExp1 + 2)) -- plus two to also skip the branch over the else statement
  ] ++ genExp1 ++ [Jump (Rel (length genExp2 + 1))] ++ genExp2
  where
    genExp1 = genExpr exp1 smTable
    genExp2 = genExpr exp2 smTable
-- Else error out
genExpr _ smTable = error "genExpr error"


-- Generate the code for doing a condition with two expressions
genCond :: Condition -> [DataBase] -> [Instruction]
genCond (Structure.Lt exp1 exp2) smTable = genTwoExpr (Sprockell.Lt,    exp1, exp2) smTable
genCond (Structure.Eq exp1 exp2) smTable = genTwoExpr (Sprockell.Equal, exp1, exp2) smTable
genCond (Structure.Gt exp1 exp2) smTable = genTwoExpr (Sprockell.Gt,    exp1, exp2) smTable
genCond (Structure.Lq exp1 exp2) smTable = genTwoExpr (Sprockell.LtE,   exp1, exp2) smTable
genCond (Structure.Gq exp1 exp2) smTable = genTwoExpr (Sprockell.GtE,   exp1, exp2) smTable

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
  result <- parseFromFile parseBlock "../examples/functest.amv"
  case result of
    Left err -> print err
    Right xs -> do
      putStrLn (pretty code) -- print code
      --run [code]
      where
        code = generation xs

pretty :: [Instruction] -> String
pretty = pretty' 0

pretty' :: Int -> [Instruction] -> String
pretty' _ [] = ""
pretty' i (x:xs) = show i ++ ":    " ++ show x ++ "\n" ++ pretty' (i+1) xs

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState

-- koekje = [Load (ImmValue 1000) 6,
-- Push 6,Compute Add 7 0 6,ComputeI Add 6 1 6,Pop 5,Store 5 (IndAddr 6),
-- Debug "FuncCall",
-- Load (ImmValue 4) 2,
-- Compute Sub 7 2 2,
-- Load (ImmValue 1) 5,
-- ComputeI Gt 5 1 6,
-- Branch 6 (Rel 7),
-- Load (IndAddr 2) 3,
-- Compute Add 7 5 6,
-- Store 3 (IndAddr 6),
-- Compute Incr 5 5 5,ComputeI Add 2 3 2,Jump (Rel (-7)),
-- Load (ImmValue 3) 6,Push 6,Compute Add 7 0 6,ComputeI Add 6 1 6,Pop 5,Load (IndAddr 6) 2,Compute Add 2 5 5,Store 5 (IndAddr 6),Compute Add 7 0 6,ComputeI Add 6 1 6,Load (IndAddr 6) 5,Push 5,Pop 6,WriteInstr 6 (DirAddr 65536),EndProg]
