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
generation xs tCount = threadStuff ++ threadBusy
  ++ genBlock commands tCount (traceShowId smTable) offset ++
            [Load (ImmValue 1) regA,
            WriteInstr regA (DirAddr 0),
            EndProg]
  where
    commands = fromBlock xs
    smTable = symbolTableBuilder commands 1 []
    threadBusy = [ComputeI Sprockell.Add regSprID 30 regB,
                  WriteInstr reg0 (IndAddr regB),
                  Jump (Abs 9)]
    offset = length threadStuff + length threadBusy
    threadStuff =
      [Branch regSprID (Rel 6) -- target "beginLoop"
      , TestAndSet (DirAddr 2)
      , Receive regE
      , Branch regE (Rel 2)
      , Jump (Rel (-3))
      , Jump (Abs offset)

      , ReadInstr (DirAddr 0)
      , Receive regB
      , Compute Equal regB reg0 regE
      , Branch regE (Rel 2)
      , EndProg -- if the main thread is done
      , TestAndSet (DirAddr 2) -- Get the read lock
      , Receive regE
      , Branch regE (Rel 2) -- if success let's continue
      , Jump (Rel (-8)) -- else we need to try to get the lock again

      , ComputeI Sprockell.Add regSprID 30 regB
      , TestAndSet (IndAddr regB) -- Check if thread is busy
      , Receive regE
      , Branch regE (Rel 2) -- if it is busy
      , Jump (Rel (-3)) -- else try again
      , ReadInstr (DirAddr 3)
      , Receive regB -- get the jump addr
      , Push regB
      , ComputeI Sprockell.Add regF 1 regC --
      , ReadInstr (DirAddr 4)
      , Receive regD
      , Load (ImmValue 5) regA
      , Compute Equal regD reg0 regE
      , Branch regE (Rel 18)
      , ReadInstr (IndAddr regA)
      , Receive regB
      , Store regB (IndAddr regC)
      , Compute Sprockell.Incr regA regA regA
      , Compute Sprockell.Incr regC regC regC
      , ReadInstr (IndAddr regA)
      , Receive regB
      , Store regB (IndAddr regC)
      , Compute Sprockell.Incr regA regA regA
      , Compute Sprockell.Incr regC regC regC
      , ReadInstr (IndAddr regA)
      , Receive regB
      , Store regB (IndAddr regC)
      , Compute Sprockell.Incr regA regA regA
      , Compute Sprockell.Incr regC regC regC
      , Compute Sprockell.Decr regD regD regD
      , Jump (Rel (-18))
      , Load (ImmValue (length threadStuff)) regD
      , Store regD (IndAddr regC)
      , Compute Sprockell.Incr regC regC regC
      , Store regF (IndAddr regC)
      , Compute Sprockell.Add regC reg0 regF
      , Pop regA
      , WriteInstr reg0 (DirAddr 1)
      , Jump (Ind regA) -- go the actual function
      ]

-- generationHelper :: [Commands] -> [DataBase] -> [Instruction]
-- -- generationHelper [] _ =
-- generationHelper xs smTable |length xs > 1 = genBlock xs smTable
--                             |otherwise = [EndProg]

-- Generate a new block. First it generates the inner functions, so you can call
--  them earlier, before they are actually defined in the AST. The first thing
--  will determine if we need to jump over the declared functions or not, and then
--- generates the main body parts.
genBlock :: [Commands] -> Int -> [DataBase] -> Int -> [Instruction]
genBlock [] _ _ _ = []
genBlock commands tCount smTable offset = jumpOverFuncs -- gen x smTable ++ genBlock xs smTable
                            ++ generatedFunctions -- TODO NOT IN HERE!!!
                            ++ mainCode ++ (traceShow fnTable) []
  where
    (functions, body) = partition isFunction commands
    (_, fnOffset, generatedFunctions, fnTable) = genFuncs functions tCount smTable offsAndJump offsAndJump []
    jumpOverFuncs = jumpOrNot (length generatedFunctions + length functions)
    (mainOffs, mainCode) = genCommands body tCount smTable fnTable (offsAndJump + length generatedFunctions)
    offsAndJump = offset + length jumpOverFuncs


genFuncs :: [Commands] -> Int -> [DataBase] -> Int -> Int -> [(String, Int)] -> (Int, Int, [Instruction] , [(String, Int)])
genFuncs [] _ _ offs fOffs fnTable = (fOffs, offs, [], fnTable)
genFuncs (x:xs) tCount smTable offs fOffs fnTable = (fOffs, finalOffset, instructions ++ otherInstructions, finalFnTable)
  where
    (newFOffs, newOffset, instructions, newFnTable) = genFunc x tCount smTable offs fOffs fnTable
    (fOffsNew, finalOffset, otherInstructions, finalFnTable) = genFuncs xs tCount smTable (newOffset + length instructions) newFOffs newFnTable

genFunc :: Commands -> Int -> [DataBase] -> Int -> Int -> [(String, Int)] -> (Int, Int, [Instruction] , [(String, Int)])
genFunc (FunDecl (Arg ftype fname) params body) tCount smTable startOffs fOffs fnTable = (newOffs, newOffs, code, (fname, fOffs) : fnTable)
     where
       newOffs = startOffs + length code
       before = [Debug ("************func: " ++ fname),
        Load (ImmValue (1 + 3 * length params)) regA,
        Compute Sub regF regA regA, -- 2. set the value of the ARP
        Load (ImmValue 1) regD, -- 3. amount of parameters already passed down

        ComputeI Sprockell.Gt regD (length params ) regE, -- 4.  LOOP check if all parameters are passed
        Branch regE (Rel 7), -- 5. then skip this parameter loading below

        Load (IndAddr regA) regB, -- 6. Load the actual parameter value
        Compute Sprockell.Add regF regD regE, -- 7. Store the address in the local data
        Store regB (IndAddr regE), -- 8.
        Compute Sprockell.Incr regD regD regD, -- 9.
        ComputeI Sprockell.Add regA 3 regA, -- 10.

        Jump (Rel (-7)),-- 11. go back to  LOOP

        Compute Sprockell.Add regF reg0 regC,
        ComputeI Sprockell.Add regC (length params + 1) regC,
        Store regF (IndAddr regC),
        Compute Sprockell.Add regC reg0 regF]

       code = before
        ++ genBlock (fromBlock body) tCount smTable (startOffs + length before)
        ++ [Load (IndAddr regF) regF, -- Go back to the old scope
          Load (ImmValue (3 * length params)) regA,
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
          Compute Sprockell.Incr regA regA regA, -- move pointr to global
          Load (IndAddr regA) regB,
          Compute Sprockell.Lt regB reg0 regE,
          Branch regE (Rel 10),

          Compute Sprockell.Add regB reg0 regE, -- lock address

          TestAndSet (IndAddr regE), -- TODO
          Receive regE, -- HERE
          Branch regE (Rel 2),
          Jump (Rel (-4)), -- go back if we cannot lock it
          ComputeI Sprockell.Add regB 1 regB,

          WriteInstr regC (IndAddr regB),
          ComputeI Sub regB 1 regB,
          WriteInstr reg0 (IndAddr regB), -- Unlock again
          Compute Sprockell.Incr  regD regD regD, --add one to D
          ComputeI Sprockell.Add regA 2 regA,
          Jump (Rel (-23)), -- Go back to the LOOP

          Compute Sprockell.Decr regF reg0 regA,
          Load (IndAddr regA) regE, -- get the return address
          Load (IndAddr regF) regF, -- restore arp
          Debug ("func: " ++ fname ++ " end! We're only going to jump!"),
          Jump (Ind regE)] -- jump to da return address
genFunc command tCount smTable offs fOffs fnTable = (fOffs, offs + length code, code, fnTable)
  where
  (offset, code) = gen command tCount smTable fnTable offs

jumpOrNot :: Int -> [Instruction]
jumpOrNot 0 = []
jumpOrNot i = [Debug "Block Jump to main:", Jump (Rel i)]

isFunction :: Commands -> Bool
isFunction FunDecl{} = True
isFunction _ = False

genCommands :: [Commands] -> Int -> [DataBase] -> [(String, Int)] -> Int -> (Int, [Instruction])
genCommands [] _ _ _ offset = (offset, [])
genCommands (x:xs) tCount smTable fnTable offs = (finalOffset, code ++ moreCode)
  where
    (newOffset, code) = gen x tCount smTable fnTable offs
    (finalOffset, moreCode) = genCommands xs tCount smTable fnTable newOffset

-- ALSO OPEN A NEW SCOPE!!!


gen :: Commands -> Int -> [DataBase] ->  [(String, Int)] -> Int -> (Int, [Instruction])
-- Generate end
gen End _ _ _ offset = (offset, [])
gen Structure.Nop _ _ _ offset = (offset, [Sprockell.Nop])
-- Generate a global variable
gen (GlobalVarDecl (Arg ftype name) expr) tCount smTable fnTable offset = genVar name expr Nothing tCount smTable fnTable offset
-- Generate a variable
gen (VarDecl (Arg varType name) expr) tCount smTable fnTable offset = genVar name expr Nothing tCount smTable fnTable offset
-- Generate a (re)assignment of a variable it's same as above
gen (Ass name expr) tCount smTable fnTable offset = genVar name expr Nothing tCount smTable fnTable offset
-- Generate a print method
gen (Print expr) tCount smTable fnTable offset = (newOffset + length code2, code1 ++ code2)
    where
      (newOffset, code1) = genExpr expr tCount smTable fnTable offset
      code2 = [Pop regE, WriteInstr regE numberIO]
-- Generate if condition
gen (IfCom cond thenBlock elseBlock) tCount smTable fnTable offset = (offset + length code, code)
  where
    code = before ++ genThen ++ [Jump (Rel (length genElse + 1))] ++ genElse
    (condOffs, beforeCond) = genCond cond tCount smTable fnTable offset
    before = beforeCond ++ [Pop regC,
      ComputeI Xor regC 1 regC, -- Branch is implemented stupidly so we need to xor (negate) it
      Branch regC (Rel (length genThen + 2))] -- plus two to also skip the branch over the else statement
    genThen = genBlock (fromBlock thenBlock) tCount smTable (offset + length before)
    genElse = genBlock (fromBlock elseBlock) tCount smTable (offset + length before + length genThen + 1) -- Plus one for the jump stuff

-- generate while statement and block
gen (While cond block) tCount smTable fnTable offset = (offset + length code, code)
  where
    code = before ++ genWhileBody ++ [Jump (Rel (-(length genWhileCond + lengthWhileBody + 3)))] -- then go back to the comparison
    before = genWhileCond ++ [Pop regC,
                              ComputeI Xor regC 1 regC, -- do this comparison again
                              Branch regC (Rel (lengthWhileBody + 2))] -- If nope, skip the entire while body
    (whileOffs, genWhileCond) = genCond cond tCount smTable fnTable offset
    genWhileBody = genBlock (fromBlock block) tCount smTable (offset + length before)
    lengthWhileBody = length genWhileBody

-- Generate code for decreasing a value
gen (Structure.Decr varName) tCount smTable fnTable offset = (offset + length code, code)
  where
    code = incrDecrVar varName Sprockell.Decr tCount smTable
-- Generate code for increasing a value
gen (Structure.Incr varName) tCount smTable fnTable offset = (offset + length code, code)
  where
    code = incrDecrVar varName Sprockell.Incr tCount smTable
-- Generate code for += statements
gen (AddCom varName expr) tCount smTable fnTable offset = genVar varName expr (Just Sprockell.Add) tCount smTable fnTable offset
-- Same as before, Generate code for -= statements
gen (MinCom varName expr) tCount smTable fnTable offset = genVar varName expr (Just Sprockell.Sub) tCount smTable fnTable offset
gen (FunCall fName params) tCount smTable fnTable offset = (offset + length code, code ++ popOrNot smTable fName) -- Dont jump over the popOrNot so didnt put that in the offset
  where
    (offs, code) = genFunctionCall fName params tCount smTable fnTable offset
-- Generate the return expression
gen (Return expr) tCount smTable fnTable offset = genExpr expr tCount smTable fnTable offset
-- Generate a new function
gen FunDecl{} _ _ _ _ = error "Generator: funDecl should not be defined in the gen method"
-- The fork method generation. All the arguments will be passed by value and the globals
--  will be referenced.
gen (Fork (Funct fName params)) tCount smTable fnTable offset = (offset + length code, code)
  where
    code = [TestAndSet (DirAddr 1), -- writelo
           Receive regE,
           Branch regE (Rel 2), -- yay
           Jump (Rel (-3))] -- no we go back to get the writelock
           ++ genFunParams params tCount smTable fnTable offset
           ++ [Load (ImmValue 5) regC]
           ++ storeForkParameters params tCount smTable
           ++ [Load (ImmValue (fnToOff fnTable fName)) regD, --Load (ImmValue (offset + length code)) regD,
           WriteInstr regD (DirAddr 3), -- write address
           WriteInstr reg0 (DirAddr 2), -- unlock read
           Load (ImmValue 1) regB, -- get write lock
           ReadInstr (IndAddr regB), -- get the write lock
           Receive regE, -- finally get it
           Branch regE (Rel 2), -- check if we have it and branch if so
           Jump (Rel (-3))]
-- Loop over all threads busy addresses
gen Join tCount smTable fnTable offset = (offset + length code, code)
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

storeParameters :: [Expr] -> Int -> [DataBase] -> [Instruction]
storeParameters [] _ _ = []
storeParameters (param:xs) tCount smTable = storeParam param tCount smTable
                                            ++ storeParameters xs tCount smTable

storeParam :: Expr -> Int -> [DataBase] -> [Instruction]
storeParam (Identifier name) tCount smTable | x >= 0 =
                                            before
                                            ++ [Compute Sprockell.Add regF reg0 regE]
                                            ++ replicate x (Load (IndAddr regE) regE)
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
                                (x, y) = getOffset2 smTable name
                                after = [Compute Sprockell.Incr regC regC regC,
                                        Load (ImmValue globalMem) regB,
                                        Store regB (IndAddr regC),
                                        Compute Sprockell.Incr regC regC regC]
                                globalMem | x >= 0 = -1
                                          | otherwise = 30 + tCount + 2*y
storeParam _ _ _ = [Pop regB, -- So now it's everything else except a reference to a variable
                    Store regB (IndAddr regC),
                    Compute Sprockell.Incr regC regC regC,
                    Load (ImmValue (-1)) regB,
                    Store regB (IndAddr regC),
                    Compute Sprockell.Incr regC regC regC,
                    Load (ImmValue (-1)) regB,
                    Store regB (IndAddr regC),
                    Compute Sprockell.Incr regC regC regC]


storeForkParameters :: [Expr] -> Int -> [DataBase] -> [Instruction]
storeForkParameters [] _ _ = []
storeForkParameters (param:xs) tCount smTable = storeForkParam param tCount smTable
                                            ++ storeForkParameters xs tCount smTable

storeForkParam :: Expr -> Int -> [DataBase] -> [Instruction]
storeForkParam (Identifier name) tCount smTable | x >= 0 =
                                            before
                                            ++ [Compute Sprockell.Add regF reg0 regE]
                                            ++ replicate x (Load (IndAddr regE) regE)
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
                                (x, y) = getOffset2 smTable name
                                after = [Compute Sprockell.Incr regC regC regC,
                                        Load (ImmValue globalMem) regB,
                                        WriteInstr regB (IndAddr regC), --
                                        Compute Sprockell.Incr regC regC regC]
                                globalMem | x >= 0 = -1
                                          | otherwise = 30 + tCount + 2*y
storeForkParam _ _ _ = [Pop regB, -- So now it's everything else except a reference to a variable
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
fnToOff ((st, i):xs) name | name == st = i
                          | otherwise = fnToOff xs name

genVar :: String -> Expr -> Maybe Operator -> Int -> [DataBase] -> [(String, Int)] -> Int -> (Int, [Instruction])
genVar name expr op tCount smTable fnTable offset | x >= 0 = (offset + length localCode, localCode)

                                   | otherwise = (offset + length globalCode, globalCode)
                            where
                              (x, y) = getOffset2 smTable name
                              (newOffs, loadVarCode) = genExpr expr tCount smTable fnTable offset -- Store value in regD
                              loadVar = loadVarCode ++ memAddr tCount smTable name (x, y) -- Store the eddress in regE
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
incrDecrVar :: String -> Operator -> Int -> [DataBase] -> [Instruction]
incrDecrVar name op tCount smTable | x >= 0 = loadVar ++ [Load (IndAddr regE) regD, -- store the actual value in regD
                                               Compute op regD regD regD,
                                               Store regD (IndAddr regE)] -- store back in regE again
                                   | otherwise =  loadVar ++ [ReadInstr (IndAddr regE),
                                                  Receive regD,
                                                  Compute op regD regD regD,
                                                  WriteInstr regD (IndAddr regE),
                                                  WriteInstr reg0 (IndAddr regA)]
                              where
                                (x, y) = getOffset2 smTable name
                                loadVar = memAddr tCount smTable name (x, y)

-- Generate an expression and push that to the stack
genExpr :: Expr -> Int -> [DataBase] -> [(String, Int)] -> Int -> (Int, [Instruction])
-- Generate constant
genExpr (Constant i) _ smTable _ offset = (offset + length code, code)
  where
  code = [Load (ImmValue (fromInteger i)) regE, Push regE]
-- Generate a boolean
genExpr (BoolConst bool) _ smTable _ offset = (offset + length code, code)
  where
  code = [Load (ImmValue (boolToInt bool)) regE, Push regE]
-- Generate the parens, just evaluate the expr in between it
genExpr (Paren expr) tCount smTable fnTable offset = genExpr expr tCount smTable fnTable offset
-- Generate a reference
genExpr (Identifier name) tCount smTable _ offset | x >= 0 = (offset + length localCode, localCode)
                                         | otherwise = (offset + length globalCode, globalCode)
                                    where
                                      (x, y) = getOffset2 smTable name
                                      loadVar = memAddr tCount smTable name (x, y)
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
genExpr (Structure.Mult exp1 exp2) tCount smTable fnTable offset = genTwoExpr (Sprockell.Mul, exp1, exp2) tCount smTable fnTable offset
genExpr (Structure.Add exp1 exp2) tCount smTable  fnTable offset = genTwoExpr (Sprockell.Add, exp1, exp2) tCount smTable fnTable offset
genExpr (Structure.Min exp1 exp2) tCount smTable  fnTable offset = genTwoExpr (Sprockell.Sub, exp1, exp2) tCount smTable fnTable offset
-- Generate an inline if statement
-- It's the same as if statement above but now with expressions instead of blocks.
genExpr (IfExpr _ cond exp1 exp2) tCount smTable fnTable offset = (offset + length code, code)
  where
    (condOffs, condCode) = genCond cond tCount smTable fnTable offset
    code = condCode ++ [Pop regC,
                          ComputeI Xor regC 1 regC, -- Branch we need to xor (negate) it
                          Branch regC (Rel (length genExp1 + 2)) -- plus two to also skip the branch over the else statement
                        ] ++ genExp1 ++ [Jump (Rel (length genExp2 + 1))] ++ genExp2
    (ex1offS, genExp1) = genExpr exp1 tCount smTable fnTable (condOffs + 3) -- + 3 for everything before genExp1
    (ex2offS, genExp2) = genExpr exp2 tCount smTable fnTable (ex1offS + 1) -- + 1
genExpr (Funct fName params) tCount smTable fnTable offset = genFunctionCall fName params tCount smTable fnTable offset
-- Else error out
genExpr _ _ _ _ _ = error "Generator: this genExpr type error not implemented"

genFunctionCall :: String -> [Expr] -> Int -> [DataBase] -> [(String, Int)] -> Int -> (Int, [Instruction])
genFunctionCall fName params tCount smTable fnTable offset = (offset + length code, code)
  where
    code = [Debug "joehoe funcall:"]
      ++ genFunParams params tCount smTable fnTable offset
      ++ [Compute  Sprockell.Add regF reg0 regC,
        ComputeI Sprockell.Add regC (length params) regC] -- pointer to save to
      ++ storeParameters params tCount smTable
      ++ [Debug "***RETURN ADDRESS!!! HERE BELOW******* (Dus na de Jump abs hier ergens benee)",
        Load (ImmValue (offset + length code)) regD, -- return address
        Store regD (IndAddr regC),
        Compute Sprockell.Incr regC regC regC,
        Store regF (IndAddr regC), -- So store the return address in regF
        Compute Sprockell.Add regC reg0 regF,  -- New scope of to the ARP
        Jump (Abs (fnToOff fnTable fName))] -- Let's jump to the actual function

-- Generate the function parameters in reversed order
genFunParams :: [Expr] -> Int -> [DataBase] -> [(String, Int)] -> Int -> [Instruction]
genFunParams [] _ _ _ _ = []
genFunParams (x:xs) tCount smTable fnTable offset = genFunParams xs tCount smTable fnTable newOffs
                                          ++ code
  where
    (newOffs, code) = genExpr x tCount smTable fnTable offset

-- Generate the code for doing a condition with two expressions
genCond :: Condition -> Int -> [DataBase] -> [(String, Int)] -> Int -> (Int, [Instruction])
genCond (Structure.Lt exp1 exp2) tCount smTable fnTable offset = genTwoExpr (Sprockell.Lt,    exp1, exp2) tCount smTable fnTable offset
genCond (Structure.Eq exp1 exp2) tCount smTable fnTable offset = genTwoExpr (Sprockell.Equal, exp1, exp2) tCount smTable fnTable offset
genCond (Structure.Gt exp1 exp2) tCount smTable fnTable offset = genTwoExpr (Sprockell.Gt,    exp1, exp2) tCount smTable fnTable offset
genCond (Structure.Lq exp1 exp2) tCount smTable fnTable offset = genTwoExpr (Sprockell.LtE,   exp1, exp2) tCount smTable fnTable offset
genCond (Structure.Gq exp1 exp2) tCount smTable fnTable offset = genTwoExpr (Sprockell.GtE,   exp1, exp2) tCount smTable fnTable offset

-- Generate a calculation of two expressions. Evaluates both expressions and pushes back the result to the stack.
genTwoExpr :: (Operator, Expr, Expr) -> Int ->  [DataBase] -> [(String, Int)] -> Int -> (Int, [Instruction])
genTwoExpr (op, exp1, exp2) tCount smTable fnTable offset = (offset2 + length code3, code1 ++ code2 ++ code3)
  where
    (offset1, code1) = genExpr exp1 tCount smTable fnTable offset
    (offset2, code2) = genExpr exp2 tCount smTable fnTable offset1
    code3 = [Pop regB, Pop regA,
             Compute op regA regB regA,
             Push regA]

-- Helper function to convert an boolean to an int
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0


-- -- Get the memory address for local or global variables
memAddr :: Int -> [DataBase] -> String -> (Int, Int) -> [Instruction]
memAddr  tCount smTable name (x, y) | x >= 0 = [Compute Sprockell.Add regF reg0 regE] -- Local
                                   ++ replicate x (Load (IndAddr regE) regE) -- So get the address in regE
                                   ++ [ComputeI Sprockell.Add regE y regE]
                        | otherwise = [Load (ImmValue globalAddress) regA, -- mr world wide.
                                      TestAndSet (IndAddr regA), -- get the Lock
                                      Receive regB,
                                      Branch regB (Rel 2),
                                      Jump (Rel (-4)), -- try again
                                      -- Store the address location in regC
                                      Load (ImmValue (globalAddress + 1)) regE] -- Now we have the lock get the actual value of the address

                        where
                          globalAddress = 30 + tCount + (2*y) -- TODO

getOffset2 :: [DataBase] -> String -> (Int, Int)
getOffset2 smTable name = (x, y)
  where
    (x', y) = getOffset smTable name
    x = x' - 1
    --y = y' + 1 -- TODO MAYBE CHANGE BACK AGAIN?

--------- DEBUG REMOVE WHEN DONE!!
codeGenTest = do
  result <- parseFromFile parseBlock "../examples/forktest.amv"
  case result of
    Left err -> print err
    Right xs -> do
      --print $ treeBuilder (fromBlock xs) 1 []
      --putStrLn (pretty code) -- print code
      run [code, code, code]
      --runWithDebugger (debuggerSimplePrint myShow) [code, code, code]
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
