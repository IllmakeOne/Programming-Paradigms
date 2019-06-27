module Generator where

import Sprockell


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

         -- transfer
         Nop,  -- transfer
         Pop regA, --(regA will be return address)
         Pop regB, -- the from parameter
         Pop regC, -- the to parameter
         Pop regD, -- the amount parameter



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
