module Generator where

import Sprockell
import Parser

-- Right (Block [VarDecl (Int "jesse") (Constant 1000),GlobalVarDecl (Int "robert") (Constant 1000),While (Eq (Identifier "robert") (Constant 0)) (Block [Incr "jesse",End]),VarDecl (Int "marieke") (Constant 5000),FunDecl (Int "transfer") [ByRef (Int "from"),ByRef (Int "to"),ByVal (Int "amount")] (Block [Incr "jesse",IfCom (Gq (Identifier "from") (Identifier "amount")) (Block [MinCom "from" (Identifier "amount"),AddCom "to" (Identifier "amount"),End]) (Block [End]),End]),FunDecl (Void "helicopterMoney") [ByVal (Int "to"),ByVal (Int "amount")] (Block [AddCom "to" (Identifier "amount"),End]),Fork (Block [FunCall "helicopterMoney" [Identifier "jesse",Constant 9000],End]),Fork (Block [FunCall "helicopterMoney" [Identifier "robert",Constant 9000],End]),Join,Print (Identifier "jesse"),End])

prog :: [Instruction]
prog = [ Load (ImmValue 1000) regA,
         Store regA (DirAddr  0),         -- int jesse = 1000 (@0)
         Load (ImmValue 1001) regA,
         Store regA (DirAddr  1),         -- int robert = 1000 (@1)

         Load (DirAddr 0) regA,
         WriteInstr regA numberIO,

         Load (DirAddr 1) regA,
         WriteInstr regA numberIO,

         -- Jump ahead to after the function(s)

         -- transfer (jesse, marieke, 100)
         Sprockell.Nop,  -- transfer
         Pop regB, --(regA will be return address)
         Pop regC, -- the from parameter DirAddr   (call by ref)
         Pop regD, -- the to parameter  DirAddr    (call by ref)
         Pop regE, -- the amount parameter just imm value

         Load (IndAddr regC) regA, -- FROM: get jesse
         -- if from >= amount
         Compute GtE regA regE regA,
         Branch regA (Rel 1), -- JUMP TO ELSE NOT TO REL 1

         Load (IndAddr regC) regA, -- FROM: get jesse
         Compute Sub regA regE regA,
         Store regA (IndAddr regC),

         Load (IndAddr regD) regA,
         Compute Sprockell.Add regA regE regA,
         Store regA (IndAddr regD),
         





         -- so when I see a function then store my address into



       -- -- "beginloop"
       -- , Compute Gt regA regE regC     -- regA > regE ?
       -- , Branch regC (Abs 13)          -- then jump to target "end"
       -- , WriteInstr regA numberIO      -- output regA
       -- , Compute Add regA regB regA
       -- , Compute Gt regB regE regC     -- regB > regE
       -- , Branch regC (Abs 13)          -- target "end"
       -- , WriteInstr regB numberIO      -- output regB
       -- , Compute Add regA regB regB
       -- , Jump (Rel (-8))               -- target "beginloop"

       -- "end"
        EndProg
       ]

-- run the prog on 1 Sprockell core
cheese = run [prog]
