module Generator where

import Sprockell
import Parser
import TreeWalker
import Structure
import Text.Parsec.String
import Data.Maybe
import Debug.Trace
import Data.List

-- Get the main program block, and generate its commands
-- tCount is the amount of threads.
generation :: Bloc -> Int -> [Instruction]
generation xs tCount = before
  ++ genBlock commands tCount smTable offset 1 []
  ++ final
  ++ [EndProg]
  where
    commands = fromBlock xs
    smTable = symbolTableBuilder commands 1 [] False
    (offset, before, final) = threadHelper tCount


threadHelper :: Int -> (Int, [Instruction], [Instruction])
threadHelper n | n <= 0 = error "We need at least one thread!"
               | n == 1 = (0, [], [])
               | otherwise = (offset, before ++ after, final)
  where
    before =
      [Branch regSprID (Rel 6), -- Go to the loop
      TestAndSet (DirAddr 2), -- otherwise unset readlock
      Receive regE,
      Branch regE (Rel 2),
      Jump (Rel (-3)),
      Jump (Abs offset), -- jump over this
      ReadInstr (DirAddr 0), -- thread loop
      Receive regB, -- check if we can finish already
      Compute Equal regB reg0 regE,
      Branch regE (Rel 2), -- check if we can finish already
      EndProg, -- we can finish
      TestAndSet (DirAddr 2), -- Get the read lock
      Receive regE,
      Branch regE (Rel 2), -- if success let's continue
      Jump (Rel (-8)), -- else we need to try to get the lock again
      ComputeI Sprockell.Add regSprID 30 regB,
      TestAndSet (IndAddr regB), -- Check if thread is busy
      Receive regE,
      Branch regE (Rel 2),
      Jump (Rel (-3)), -- else try again
      ReadInstr (DirAddr 3),
      Receive regB, -- get the jump addr
      Push regB,
      ComputeI Sprockell.Add regF 1 regC, -- increase the AR
      ReadInstr (DirAddr 4),
      Receive regD, -- Get the amount of parameters
      Load (ImmValue 5) regA, -- The first parameter
      Compute Equal regD reg0 regE, -- parameter passing loop
      Branch regE (Rel 18),
      ReadInstr (IndAddr regA), -- read the parameter
      Receive regB,
      Store regB (IndAddr regC), -- Store in local
      Compute Sprockell.Incr regA regA regA,
      Compute Sprockell.Incr regC regC regC, -- setting the offsets to get globals
      ReadInstr (IndAddr regA),
      Receive regB,
      Store regB (IndAddr regC), -- put them to in local
      Compute Sprockell.Incr regA regA regA,
      Compute Sprockell.Incr regC regC regC, -- Setting offsets for local parameters
      ReadInstr (IndAddr regA),
      Receive regB,
      Store regB (IndAddr regC), -- store them
      Compute Sprockell.Incr regA regA regA,
      Compute Sprockell.Incr regC regC regC,
      Compute Sprockell.Decr regD regD regD, -- next args
      Jump (Rel (-18)), -- check if we still need to pass parameters
      Load (ImmValue (length before)) regD,
      Store regD (IndAddr regC), -- Set the return address
      Compute Sprockell.Incr regC regC regC,
      Store regF (IndAddr regC), -- get AR
      Compute Sprockell.Add regC reg0 regF, -- increase it
      Pop regA,
      WriteInstr reg0 (DirAddr 1),
      Jump (Ind regA)] -- go the actual fork function
    after = [ComputeI Sprockell.Add regSprID 30 regB, -- after the fork
            WriteInstr reg0 (IndAddr regB),
            Jump (Abs 9)] -- Jump to the thread loop
    final = [Load (ImmValue 1) regA,
            WriteInstr regA (DirAddr 0)]
    offset = length before + length after

-- Generate a new block. First it generates the inner functions, so you can call
--  them earlier, before they are actually defined in the AST. The first thing
--  will determine if we need to jump over the declared functions or not, and then
--- generates the main body commands, and an offset can be set .
genBlock :: [Commands] -> Int -> [DataBase] -> Int -> Int -> [(String, Int)] -> [Instruction]
genBlock [] _ _ _ _ _ = []
genBlock commands tCount smTable offset scope oldFnTable = jumpOverFuncs
                            ++ generatedFunctions
                            ++ mainCode -- ++ (traceShow smTable) []
  where
    (functions, body) = partition isFunction commands
    (_, fnOffset, generatedFunctions, fnTable) = genFuncs functions tCount smTable offsAndJump offsAndJump (scope+1) []
    jumpOverFuncs = jumpOrNot (length generatedFunctions + jumpFuncs (length functions))
    (mainOffs, mainCode) = genCommands body tCount smTable (oldFnTable ++ fnTable) (offsAndJump + length generatedFunctions) (scope) -- scope +1
    offsAndJump = offset + length jumpOverFuncs
    jumpFuncs len | len > 0 = len - (len-1) -- for nested functions need to remove actually
                  | otherwise = 0

-- Helper function to return the function declarations and their offsets, and passing the offsets and function table.
genFuncs :: [Commands] -> Int -> [DataBase] -> Int -> Int -> Int -> [(String, Int)] -> (Int, Int, [Instruction] , [(String, Int)])
genFuncs [] _ _ offs fOffs _ fnTable = (fOffs, offs, [], fnTable)
genFuncs (x:xs) tCount smTable offs fOffs scope fnTable = (fOffs, finalOffset, instructions ++ otherInstructions, finalFnTable)
  where
    (newFOffs, newOffset, instructions, newFnTable) = genFunc x tCount smTable offs fOffs scope fnTable
    (fOffsNew, finalOffset, otherInstructions, finalFnTable) = genFuncs xs tCount smTable (newOffset + length instructions) newFOffs scope newFnTable
-- Generate a function and update the function table.
genFunc :: Commands -> Int -> [DataBase] -> Int -> Int -> Int -> [(String, Int)] -> (Int, Int, [Instruction] , [(String, Int)])
genFunc (FunDecl (Arg ftype fname) params body) tCount smTable startOffs fOffs scope fnTable = (newOffs, fOffs, code, (fname, fOffs) : fnTable)
     where
       newOffs = startOffs + length code
       before = [Debug ("************func prologue: " ++ fname),
        Load (ImmValue (1 + 3 * length params)) regA, -- get right offset for AR
        Compute Sub regF regA regA, -- set it
        Load (ImmValue 1) regD,
        ComputeI Sprockell.Gt regD (length params ) regE, -- parameter passing LOOP
        Branch regE (Rel 7), -- then skip this parameter loading below
        Load (IndAddr regA) regB, -- Load the parameter
        Compute Sprockell.Add regF regD regE,
        Store regB (IndAddr regE), -- store it local
        Compute Sprockell.Incr regD regD regD, -- next arg
        ComputeI Sprockell.Add regA 3 regA, -- 10.
        Jump (Rel (-7)),-- Back to loop
        Compute Sprockell.Add regF reg0 regC, -- Set the new AR
        ComputeI Sprockell.Add regC (length params + 1) regC,
        Store regF (IndAddr regC), -- go pass the params
        Compute Sprockell.Add regC reg0 regF,
        Debug "Now the code"] -- reset back
       code = before
        ++ genBlock (fromBlock body) tCount smTable (startOffs + length before) scope fnTable
        ++ [Debug ("**fun epilogue: " ++ fname),
          Load (IndAddr regF) regF, -- Go back to the old scope
          Load (ImmValue (3 * length params)) regA,
          Compute Sub regF regA regA,
          ComputeI Sprockell.Add reg0 1 regD,
          ComputeI Sprockell.Gt regD (length params) regE, -- LOOP reset the variables for call by ref
          Branch regE (Rel 23), -- check if we're done all the params or not
          Compute Sprockell.Add regF regD regE, -- Getting the current var, and the previous
          Load (IndAddr regE) regC,
          Load (IndAddr regA) regB,
          Compute Sprockell.Lt regB reg0 regE,
          Branch regE (Rel 2),
          Store regC (IndAddr regB), -- Save the current to the previous
          Compute Sprockell.Incr regA regA regA, -- Now do the same for globals
          Load (IndAddr regA) regB,
          Compute Sprockell.Lt regB reg0 regE,
          Branch regE (Rel 10),
          Compute Sprockell.Add regB reg0 regE,
          TestAndSet (IndAddr regE), -- lock global
          Receive regE,
          Branch regE (Rel 2),
          Jump (Rel (-4)), -- go back if we cannot lock it
          ComputeI Sprockell.Add regB 1 regB,
          WriteInstr regC (IndAddr regB),
          ComputeI Sub regB 1 regB, -- get the actual value ref
          WriteInstr reg0 (IndAddr regB), -- Unlock
          Compute Sprockell.Incr  regD regD regD, -- next var
          ComputeI Sprockell.Add regA 2 regA,
          Jump (Rel (-23)), -- Go back to the LOOP
          Compute Sprockell.Decr regF reg0 regA, -- Now let's go back
          Load (IndAddr regA) regE, -- get the return address
          Load (IndAddr regF) regF, -- restore arp
          Debug ("func: " ++ fname ++ " end! We're only going to jump!"),
          Jump (Ind regE)] -- jump to da return address
-- If we don't actually pass an function, just generate the command that we would anyway
genFunc command tCount smTable offs fOffs scope fnTable = (fOffs, offs + length code, code, fnTable)
  where
  (offset, code) = gen command tCount smTable fnTable offs scope

-- Determines if we need to jump based on a given offset
jumpOrNot :: Int -> [Instruction]
jumpOrNot 0 = []
jumpOrNot i = [Debug "Block Jump to main:", Jump (Rel i)]

-- Determines if we are generating a funcdecl. Needed to filter them out in genBlock.
isFunction :: Commands -> Bool
isFunction FunDecl{} = True
isFunction _ = False

-- Just generate a ordinary list of commands with the helper variables, and return the new offset and its instructions.
genCommands :: [Commands] -> Int -> [DataBase] -> [(String, Int)] -> Int -> Int -> (Int, [Instruction])
genCommands [] _ _ _ offset _ = (offset, [])
genCommands (x:xs) tCount smTable fnTable offs scope = (finalOffset, code ++ moreCode)
  where
    (newOffset, code) = gen x tCount smTable fnTable offs scope
    (finalOffset, moreCode) = genCommands xs tCount smTable fnTable newOffset scope

-- Just generate one command with the helper variables and returns a new offset and its instructions.
gen :: Commands -> Int -> [DataBase] ->  [(String, Int)] -> Int -> Int -> (Int, [Instruction])
-- Generate end
gen End _ _ _ offset _ = (offset, [])
gen Structure.Nop _ _ _ offset _ = (offset, [Sprockell.Nop])
-- Generate a global variable
gen (GlobalVarDecl (Arg ftype name) expr) tCount smTable fnTable offset scope = genVar name expr Nothing True tCount smTable fnTable offset scope
-- Generate a variable
gen (VarDecl (Arg varType name) expr) tCount smTable fnTable offset scope = genVar name expr Nothing False tCount smTable fnTable offset scope
-- Generate a (re)assignment of a variable it's same as above
gen (Ass name expr) tCount smTable fnTable offset scope = genVar name expr Nothing False tCount smTable fnTable offset scope
-- Generate a print method
gen (Print expr) tCount smTable fnTable offset scope = (newOffset + length code2, code1 ++ code2)
    where
      (newOffset, code1) = genExpr expr tCount smTable fnTable offset scope
      code2 = [Pop regE, WriteInstr regE numberIO]
-- Generate if condition
gen (IfCom cond thenBlock elseBlock) tCount smTable fnTable offset scope = (offset + length code, code)
  where
    code = before ++ genThen ++ [Jump (Rel (length genElse + 1))] ++ genElse
    (condOffs, beforeCond) = genCond cond tCount smTable fnTable offset scope
    before = beforeCond ++ [Pop regC,
                            BranchX regC (Rel (length genThen + 2))] -- plus two to also skip the branch over the else statement
    genThen = genBlock (fromBlock thenBlock) tCount smTable (offset + length before) scope fnTable
    genElse = genBlock (fromBlock elseBlock) tCount smTable (offset + length before + length genThen + 1) scope fnTable -- Plus one for the jump stuff

-- generate while statement and block
gen (While cond block) tCount smTable fnTable offset scope = (offset + length code, code)
  where
    code = before ++ genWhileBody ++ [Jump (Rel (-(length genWhileCond + lengthWhileBody + 3)))] -- then go back to the comparison
    before = genWhileCond ++ [Pop regC,
                              BranchX regC (Rel (lengthWhileBody + 2))] -- If nope, skip the entire while body
    (whileOffs, genWhileCond) = genCond cond tCount smTable fnTable offset scope
    genWhileBody = genBlock (fromBlock block) tCount smTable (offset + length before) scope fnTable
    lengthWhileBody = length genWhileBody

-- Generate code for decreasing a value
gen (Structure.Decr varName) tCount smTable fnTable offset scope = (offset + length code, code)
  where
    code = incrDecrVar varName Sprockell.Decr tCount smTable scope
-- Generate code for increasing a value
gen (Structure.Incr varName) tCount smTable fnTable offset scope = (offset + length code, code)
  where
    code = incrDecrVar varName Sprockell.Incr tCount smTable scope
-- Generate code for += statements
gen (AddCom varName expr) tCount smTable fnTable offset scope = genVar varName expr (Just Sprockell.Add) False tCount  smTable fnTable offset scope
-- Same as before, Generate code for -= statements
gen (MinCom varName expr) tCount smTable fnTable offset scope = genVar varName expr (Just Sprockell.Sub) False tCount smTable fnTable offset scope
gen (FunCall fName params) tCount smTable fnTable offset scope = (offset + length code, code ++ popOrNot smTable fName) -- Dont jump over the popOrNot so didnt put that in the offset
  where
    (offs, code) = genFunctionCall fName params tCount smTable fnTable offset scope
-- Generate the return expression
gen (Return expr) tCount smTable fnTable offset scope = genExpr expr tCount smTable fnTable offset scope
-- Generate a new function
gen FunDecl{} _ _ _ _ _ = error "Generator: funDecl should not be defined in the gen method"
-- The fork method generation. All the arguments will be passed by value and the globals
--  will be referenced.
gen (Fork (Funct fName params)) tCount smTable fnTable offset scope | tCount <= 1 = error "Fork error: We need at least two threads!"
                                                                    | otherwise = (offset + length code, code)
  where
    code = [TestAndSet (DirAddr 1), -- writelo
           Receive regE,
           Branch regE (Rel 2), -- yay
           Jump (Rel (-3))] -- no we go back to get the writelock
           ++ genFunParams params tCount smTable fnTable offset scope
           ++ [Load (ImmValue 5) regC]
           ++ storeForkParameters params tCount smTable scope
           ++ [Debug "Forkie",
           Load (ImmValue (length params)) regD,
           WriteInstr regD (DirAddr 4),
           Load (ImmValue (fnToOff fnTable fName)) regD,
           WriteInstr regD (DirAddr 3), -- write address
           WriteInstr reg0 (DirAddr 2), -- unlock read
           Load (ImmValue 1) regB, -- get write lock
           ReadInstr (IndAddr regB), -- get the write lock
           Receive regE, -- finally get it
           Branch regE (Rel 2), -- check if we have it and branch if so
           Jump (Rel (-3))]
-- Loop over all threads busy addresses
gen Join tCount smTable fnTable offset scope = (offset + length code, code)
  where
    code =  [Compute Equal reg0 regSprID regE,
            Branch regE (Rel 3),
            Load (ImmValue 2) regA,
            EndProg,
            Load (ImmValue 30) regB,
            Load (ImmValue 0) regA,
            ReadInstr (IndAddr regB),
            Receive regC,
            Compute Sprockell.Add regA regC regA,
            ComputeI NEq regB (30 + tCount) regE,
            Compute Sprockell.Incr regB regB regB,
            Branch regE (Rel (-5)),
            Compute Equal regA reg0 regE,
            Branch regE (Rel 2),
            Jump (Rel (-10))]
popOrNot :: [DataBase] -> String -> [Instruction]
popOrNot smTable fName | funcPoppable smTable fName = [Pop reg0]
                       | otherwise = []

funcPoppable :: [DataBase] -> String -> Bool
funcPoppable [] _ = error "Generator:PopOrNot:funcPoppable: didn't find func"
funcPoppable (DBF (Arg SimplyNull fName) _ _:xs) name | fName == name = False
                                                      | otherwise = funcPoppable xs name
funcPoppable (DBF (Arg _ fName) _ _:xs) name | fName == name = True
                                             | otherwise = funcPoppable xs name
funcPoppable (DB{}:xs) name = funcPoppable xs name

storeParameters :: [Expr] -> Int -> [DataBase] -> Int -> [Instruction]
storeParameters [] _ _ _ = []
storeParameters (param:xs) tCount smTable scope = storeParam param tCount smTable scope
                                                ++ storeParameters xs tCount smTable scope

storeParam :: Expr -> Int -> [DataBase] -> Int -> [Instruction]
storeParam (Identifier name) tCount smTable scope | x >= 0 =
                                            before
                                            ++ [Compute Sprockell.Add regF reg0 regE]
                                            ++ replicate (x+scope) (Load (IndAddr regE) regE)
                                            ++ [ComputeI Sprockell.Add regE (y+1) regE,
                                                Store regE (IndAddr regC)]
                                            ++ after
                                            | otherwise =
                                                before
                                                ++ [Load (ImmValue (-1)) regB,
                                                    Store regB (IndAddr regC)]
                                                ++ after
                              where
                                before = [Pop regB,
                                          Store regB (IndAddr regC),
                                          Compute Sprockell.Incr regC regC regC]
                                (x, y, isFnParam) = getOffset2 smTable name scope
                                after = [Compute Sprockell.Incr regC regC regC,
                                        Load (ImmValue globalMem) regB,
                                        Store regB (IndAddr regC),
                                        Compute Sprockell.Incr regC regC regC]
                                globalMem | x >= 0 = -1
                                          | otherwise = 30 + tCount + 2*y
                                replicateAmount x scope | x == scope = 0
                                                        | otherwise = x-(scope-2)
storeParam _ _ _ _ = [Pop regB, -- So now it's everything else except a reference to a variable
                    Store regB (IndAddr regC),
                    Compute Sprockell.Incr regC regC regC,
                    Load (ImmValue (-1)) regB,
                    Store regB (IndAddr regC),
                    Compute Sprockell.Incr regC regC regC,
                    Load (ImmValue (-1)) regB,
                    Store regB (IndAddr regC),
                    Compute Sprockell.Incr regC regC regC]


storeForkParameters :: [Expr] -> Int -> [DataBase] -> Int -> [Instruction]
storeForkParameters [] _ _ _ = []
storeForkParameters (param:xs) tCount smTable scope = storeForkParam param tCount smTable scope
                                                    ++ storeForkParameters xs tCount smTable scope

storeForkParam :: Expr -> Int -> [DataBase] -> Int -> [Instruction]
storeForkParam (Identifier name) tCount smTable scope | x >= 0 =
                                                        before
                                                        ++ [Compute Sprockell.Add regF reg0 regE]
                                                        ++ replicate (x+scope) (Load (IndAddr regE) regE)
                                                        ++ [ComputeI Sprockell.Add regE (y+1) regE,
                                                            WriteInstr regE (IndAddr regC)] -- conc
                                                        ++ after
                                                        | otherwise =
                                                            before
                                                            ++ [Load (ImmValue (-1)) regB,
                                                            WriteInstr regB (IndAddr regC)] -- conc
                                                            ++ after
                              where
                                before = [Pop regB,
                                          WriteInstr regB (IndAddr regC), --
                                          Compute Sprockell.Incr regC regC regC]
                                (x, y, isFnParam) = getOffset2 smTable name scope
                                after = [Compute Sprockell.Incr regC regC regC,
                                        Load (ImmValue globalMem) regB,
                                        WriteInstr regB (IndAddr regC), --
                                        Compute Sprockell.Incr regC regC regC]
                                globalMem | x >= 0 = -1
                                          | otherwise = 30 + tCount + 2*y
storeForkParam _ _ _ _ = [Pop regB, -- So now it's everything else except a reference to a variable
                        WriteInstr regB (IndAddr regC), -- conc
                        Compute Sprockell.Incr regC regC regC,
                        Load (ImmValue (-1)) regB,
                        WriteInstr regB (IndAddr regC), -- conc
                        Compute Sprockell.Incr regC regC regC,
                        Load (ImmValue (-1)) regB,
                        WriteInstr regB (IndAddr regC), -- conc
                        Compute Sprockell.Incr regC regC regC]
-- Small helper function to retrieve the offset of a function given its name and
-- the functionTable.
fnToOff :: [(String, Int)] -> String -> Int
fnToOff [] st = error $ "Generator: Cannot find function: " ++ st
fnToOff ((st, i):xs) name | name == st = i --(traceShow (st, i)) i
                          | otherwise = fnToOff xs name

genVar :: String -> Expr -> Maybe Operator -> Bool -> Int -> [DataBase] -> [(String, Int)] -> Int -> Int-> (Int, [Instruction])
genVar name expr op globDecl tCount smTable fnTable offset scope | x >= 0 = (offset + length localCode, localCode)
                                                                 | otherwise = (offset + length globalCode, globalCode)
                            where
                              (x, y, isFnParam) = getOffset2 smTable name scope
                              (newOffs, loadVarCode) = genExpr expr tCount smTable fnTable offset scope -- Store value in regD
                              loadVar = loadVarCode ++ memAddr tCount globDecl smTable name scope (x, y, isFnParam) -- Store the eddress in regE
                                        ++ [Pop regD]
                                        ++ genVarCompute op (x, y)
                              localCode  = loadVar ++ [Store regD (IndAddr regE)]
                              globalCode = loadVar
                                            ++ [WriteInstr regD (IndAddr regE),
                                            WriteInstr reg0 (IndAddr regA)] -- Unlock


-- Do we want to compute something with the variable? This helper function will
-- find out.
genVarCompute :: Maybe Operator -> (Int, Int) -> [Instruction]
genVarCompute Nothing _ = []
genVarCompute (Just op) (x, y) | x >= 0 = [Load (IndAddr regE) regC, -- load the value of the address in regC
                                          Compute op regC regD regD] -- Store the evaluation in regD
                               | otherwise = [ReadInstr (IndAddr regE), -- Ask to get the actual value
                                              Receive regC, -- Put that in regC
                                              Compute op regC regD regD] -- Do also here the calculation

-- Function to just get the value from memory and increase/ decrease based on
-- the operator. The caller of this function has to make sure to only use Incr/ Decr.
incrDecrVar :: String -> Operator -> Int -> [DataBase] -> Int -> [Instruction]
incrDecrVar name op tCount smTable scope | x >= 0 = loadVar ++ [Load (IndAddr regE) regD, -- store the actual value in regD
                                               Compute op regD regD regD,
                                               Store regD (IndAddr regE)] -- store back in regE again
                                   | otherwise =  loadVar ++ [ReadInstr (IndAddr regE),
                                                  Receive regD,
                                                  Compute op regD regD regD,
                                                  WriteInstr regD (IndAddr regE),
                                                  WriteInstr reg0 (IndAddr regA)]
                              where
                                (x, y, isFnParam) = getOffset2 smTable name scope
                                loadVar = memAddr tCount False smTable name scope (x, y, isFnParam)

-- Generate an expression and push that to the stack
genExpr :: Expr -> Int -> [DataBase] -> [(String, Int)] -> Int -> Int -> (Int, [Instruction])
-- Generate constant
genExpr (Constant i) _ smTable _ offset scope = (offset + length code, code)
  where
  code = [Load (ImmValue (fromInteger i)) regE, Push regE]
-- Generate a boolean
genExpr (BoolConst bool) _ smTable _ offset scope = (offset + length code, code)
  where
  code = [Load (ImmValue (boolToInt bool)) regE, Push regE]
-- Generate the parens, just evaluate the expr in between it
genExpr (Paren expr) tCount smTable fnTable offset scope = genExpr expr tCount smTable fnTable offset scope
-- Generate a reference
genExpr (Identifier name) tCount smTable _ offset scope | x >= 0 = (offset + length localCode, localCode)
                                                        | otherwise = (offset + length globalCode, globalCode)
                                    where
                                      (x, y, isFnParam) = getOffset2 smTable name scope
                                      loadVar = memAddr tCount False smTable name scope (x, y, isFnParam)
                                      globalAddress = 30 + tCount + 2*y
                                      localCode = loadVar
                                                  ++ [Load (IndAddr regE) regD,
                                                      Push regD]
                                      globalCode = [Load (ImmValue globalAddress) regA, -- Get da lock
                                                    TestAndSet (IndAddr regA),
                                                    Receive regB,
                                                    Branch regB (Rel 2),
                                                    Jump (Rel (-4)),
                                                    Load (ImmValue (globalAddress + 1)) regC, -- Individual Lock
                                                    ReadInstr (IndAddr regC),
                                                    Receive regD,
                                                    Push regD,
                                                    WriteInstr reg0 (IndAddr regA)] -- Unlock again
-- Generate a calculation of two expressions
genExpr (Structure.Mult exp1 exp2) tCount smTable fnTable offset scope = genTwoExpr (Sprockell.Mul, exp1, exp2) tCount smTable fnTable offset scope
genExpr (Structure.Add exp1 exp2) tCount smTable  fnTable offset scope = genTwoExpr (Sprockell.Add, exp1, exp2) tCount smTable fnTable offset scope
genExpr (Structure.Min exp1 exp2) tCount smTable  fnTable offset scope = genTwoExpr (Sprockell.Sub, exp1, exp2) tCount smTable fnTable offset scope
-- Generate an inline if statement
-- It's the same as if statement above but now with expressions instead of blocks.
genExpr (IfExpr _ cond exp1 exp2) tCount smTable fnTable offset scope = (offset + length code, code)
  where
    (condOffs, condCode) = genCond cond tCount smTable fnTable offset scope
    code = condCode ++ [Pop regC,
                        BranchX regC (Rel (length genExp1 + 2))] -- plus two to also skip the branch over the else statement
                       ++ genExp1 ++ [Jump (Rel (length genExp2 + 1))] ++ genExp2
    (ex1offS, genExp1) = genExpr exp1 tCount smTable fnTable (condOffs + 2) scope -- + 2 for everything before genExp1
    (ex2offS, genExp2) = genExpr exp2 tCount smTable fnTable (ex1offS + 1) scope -- + 1
genExpr (Funct fName params) tCount smTable fnTable offset scope = genFunctionCall fName params tCount smTable fnTable offset scope
genExpr NullExpr _ _ _ offset _ = (offset + 1, [Sprockell.Nop])

-- Helper function to return the function calls.
genFunctionCall :: String -> [Expr] -> Int -> [DataBase] -> [(String, Int)] -> Int -> Int -> (Int, [Instruction])
genFunctionCall fName params tCount smTable fnTable offset scope = (offset + length code, code)
  where
    code = [Debug "funcall (precall):"]
      ++ genFunParams params tCount smTable fnTable offset scope
      ++ [Compute  Sprockell.Add regF reg0 regC, -- set the AR in C
        ComputeI Sprockell.Add regC (length params + 1) regC] -- pointer to save to
      ++ storeParameters params tCount smTable scope
      ++ [Debug "***RETURN ADDRESS:",
        Load (ImmValue (offset + length code)) regD, -- return address
        Store regD (IndAddr regC),
        Compute Sprockell.Incr regC regC regC,
        Store regF (IndAddr regC), -- So store the return address
        Compute Sprockell.Add regC reg0 regF,  -- New scope of to the F
        Debug "Here we goo:",
        Jump (Abs (fnToOff fnTable fName))] -- Let's jump to the actual function

-- Generate the function parameters in reversed order
genFunParams :: [Expr] -> Int -> [DataBase] -> [(String, Int)] -> Int -> Int -> [Instruction]
genFunParams [] _ _ _ _ _ = []
genFunParams (x:xs) tCount smTable fnTable offset scope = genFunParams xs tCount smTable fnTable newOffs scope
                                                        ++ code
  where
    (newOffs, code) = genExpr x tCount smTable fnTable offset scope

-- Generate the code for doing a condition with two expressions
genCond :: Condition -> Int -> [DataBase] -> [(String, Int)] -> Int -> Int -> (Int, [Instruction])
genCond (Structure.Lt exp1 exp2) tCount smTable fnTable offset scope = genTwoExpr (Sprockell.Lt,    exp1, exp2) tCount smTable fnTable offset scope
genCond (Structure.Eq exp1 exp2) tCount smTable fnTable offset scope = genTwoExpr (Sprockell.Equal, exp1, exp2) tCount smTable fnTable offset scope
genCond (Structure.Gt exp1 exp2) tCount smTable fnTable offset scope = genTwoExpr (Sprockell.Gt,    exp1, exp2) tCount smTable fnTable offset scope
genCond (Structure.Lq exp1 exp2) tCount smTable fnTable offset scope = genTwoExpr (Sprockell.LtE,   exp1, exp2) tCount smTable fnTable offset scope
genCond (Structure.Gq exp1 exp2) tCount smTable fnTable offset scope = genTwoExpr (Sprockell.GtE,   exp1, exp2) tCount smTable fnTable offset scope

-- Generate a calculation of two expressions. Evaluates both expressions and pushes back the result to the stack.
genTwoExpr :: (Operator, Expr, Expr) -> Int ->  [DataBase] -> [(String, Int)] -> Int -> Int -> (Int, [Instruction])
genTwoExpr (op, exp1, exp2) tCount smTable fnTable offset scope = (offset2 + length code3, code1 ++ code2 ++ code3)
  where
    (offset1, code1) = genExpr exp1 tCount smTable fnTable offset scope
    (offset2, code2) = genExpr exp2 tCount smTable fnTable offset1 scope
    code3 = [Pop regB, Pop regA,
             Compute op regA regB regA,
             Push regA]

-- Helper function to convert an boolean to an int
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0


-- -- Get the memory address for local or global variables
memAddr :: Int -> Bool -> [DataBase] -> String -> Int -> (Int, Int, Bool) -> [Instruction]
memAddr  tCount globDecl smTable name scope (x, y, isFnParam) | x >= 0 = [Compute Sprockell.Add regF reg0 regE] -- Local
                                   ++ [Debug ("***xje:" ++ name ++ "! scope:" ++ show scope ++ ", x:" ++ show x ++ ", isFnParam: " ++ show isFnParam) ]
                                   ++ replicate (replicateAmount x scope isFnParam) (Load (IndAddr regE) regE) -- (x-scope) So get the address in regE
                                   ++ [ComputeI Sprockell.Add regE y regE]
                        | otherwise = [Load (ImmValue globalAddress) regA, -- mr world wide.
                                      TestAndSet (IndAddr regA), -- get the Lock
                                      Receive regB,
                                      Branch regB (Rel 2),
                                      Jump (Rel (jumpTo globDecl)), -- try again
                                      -- Store the address location in regC
                                      Load (ImmValue (globalAddress + 1)) regE] -- Now we have the lock get the actual value of the address

                        where
                          -- plusOneDecl decl | decl = 0
                          --                  | otherwise = -1
                          globalAddress = 30 + tCount + (2*y)
                          jumpTo globDecl | globDecl = -3
                                          | otherwise = -4
                          replicateAmount x scope isFnParam | not isFnParam && scope == (x+1) = 0
                                                            | x == scope = 0
                                                            | otherwise = x-(scope-2)
-- Get it's offset, but reverse the list to get the latest offset.
getOffset2 :: [DataBase] -> String -> Int -> (Int, Int, Bool)
getOffset2 smTable name scope = (x, y, isFnParam) -- traceShow (scope, name, x, y)
  where
    (x', y, isFnParam) = getOffset smTable name scope
    x = x' - 2 + (scope-1)

--------- DEBUG REMOVE WHEN DONE!!
codeGenTest = do
  result <- parseFromFile parseBlock "../examples/functest.amv"
  case result of
    Left err -> print err
    Right xs -> do
      --print $ treeBuilder (fromBlock xs) 1 []
      -- putStrLn (pretty code) -- print code
      run $ replicate threadAmount code
      --runWithDebugger (debuggerSimplePrint myShow) $ replicate threadAmount code
      where
        code = generation xs threadAmount
        threadAmount = 3 -- <<<<<<<<<<<<<<<<<<<<<

pretty :: [Instruction] -> String
pretty = pretty' 0

pretty' :: Int -> [Instruction] -> String
pretty' _ [] = ""
pretty' i (x:xs) = show i ++ ":    " ++ show x ++ "\n" ++ pretty' (i+1) xs

showLocalMem :: DbgInput -> String
showLocalMem ( _ , systemState ) = show $ localMem $ head $ sprStates systemState
