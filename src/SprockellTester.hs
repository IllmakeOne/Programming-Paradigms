import Sprockell

main = do
  run $ replicate 3 [Branch 1 (Rel 6)
    ,TestAndSet (DirAddr 2)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-3))
    ,Jump (Rel 523)
    ,ReadInstr (DirAddr 0)
    ,Receive 3
    ,Compute Equal 3 0 6
    ,Branch 6 (Rel 2)
    ,EndProg
    ,TestAndSet (DirAddr 2)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-8))
    ,ComputeI Add 1 30 3
    ,TestAndSet (IndAddr 3)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-3))
    ,ReadInstr (DirAddr 3)
    ,Receive 3
    ,Push 3
    ,ComputeI Add 7 1 4
    ,ReadInstr (DirAddr 4)
    ,Receive 5
    ,Load (ImmValue 5) 2
    ,Compute Equal 5 0 6
    ,Branch 6 (Rel 18)
    ,ReadInstr (IndAddr 2)
    ,Receive 3
    ,Store 3 (IndAddr 4)
    ,Compute Incr 2 0 2
    ,Compute Incr 4 0 4
    ,ReadInstr (IndAddr 2)
    ,Receive 3
    ,Store 3 (IndAddr 4)
    ,Compute Incr 2 0 2
    ,Compute Incr 4 0 4
    ,ReadInstr (IndAddr 2)
    ,Receive 3
    ,Store 3 (IndAddr 4)
    ,Compute Incr 2 0 2
    ,Compute Incr 4 0 4
    ,Compute Decr 5 0 5
    ,Jump (Rel (-18))
    ,Load (ImmValue 54) 5
    ,Store 5 (IndAddr 4)
    ,Compute Incr 4 0 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Pop 2
    ,WriteInstr 0 (DirAddr 1)
    ,Jump (Ind 2)
    ,ComputeI Add 1 30 3
    ,WriteInstr 0 (IndAddr 3)
    ,Jump (Abs 9)
    ,Load (ImmValue 1) 2
    ,Compute Sub 7 2 2
    ,Load (ImmValue 1) 5
    ,ComputeI Gt 5 0 6
    ,Branch 6 (Rel 7)
    ,Load (IndAddr 2) 3
    ,Compute Add 7 5 6
    ,Store 3 (IndAddr 6)
    ,Compute Incr 5 0 5
    ,ComputeI Add 2 3 2
    ,Jump (Rel (-7))
    ,Compute Add 7 0 4
    ,ComputeI Add 4 1 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (ImmValue 1) 6
    ,Push 6
    ,Load (ImmValue 33) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 34) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 1) 6
    ,Push 6
    ,Load (ImmValue 39) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 40) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 35) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 36) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 39) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 40) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 1) 6
    ,Push 6
    ,Pop 3
    ,Pop 2
    ,Compute Equal 2 3 4
    ,Push 4
    ,Pop 3
    ,Pop 2
    ,Compute And 2 3 4
    ,Push 4
    ,Pop 6
    ,ComputeI Xor 6 1 6
    ,Branch 6 (Rel 7)
    ,Compute Add 7 0 4
    ,ComputeI Add 4 1 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (IndAddr 7) 7
    ,Jump (Rel (-38))
    ,Load (ImmValue 5) 6
    ,Push 6
    ,Compute Add 7 0 6
    ,ComputeI Add 6 1 6
    ,Pop 5
    ,Store 5 (IndAddr 6)
    ,Compute Add 7 0 6
    ,ComputeI Add 6 1 6
    ,Load (IndAddr 6) 5
    ,Push 5
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 3
    ,Pop 2
    ,Compute Gt 2 3 4
    ,Push 4
    ,Pop 6
    ,ComputeI Xor 6 1 6
    ,Branch 6 (Rel 43)
    ,Compute Add 7 0 4
    ,ComputeI Add 4 2 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (ImmValue 37) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 38) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Pop 2
    ,Compute Incr 2 0 4
    ,Push 4
    ,Load (ImmValue 37) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 38) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Compute Add 7 0 6
    ,Load (IndAddr 6) 6
    ,ComputeI Add 6 1 6
    ,Load (IndAddr 6) 5
    ,Push 5
    ,Pop 2
    ,Compute Decr 2 0 4
    ,Push 4
    ,Compute Add 7 0 6
    ,Load (IndAddr 6) 6
    ,ComputeI Add 6 1 6
    ,Pop 2
    ,Store 2 (IndAddr 6)
    ,Push 2
    ,Load (IndAddr 7) 7
    ,Jump (Rel (-54))
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Load (ImmValue 33) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 34) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (IndAddr 7) 7
    ,Load (ImmValue 0) 2
    ,Compute Sub 7 2 2
    ,ComputeI Add 0 1 5
    ,ComputeI Gt 5 0 6
    ,Branch 6 (Rel 23)
    ,Compute Add 7 5 6
    ,Load (IndAddr 6) 4
    ,Load (IndAddr 2) 3
    ,Compute Lt 3 0 6
    ,Branch 6 (Rel 2)
    ,Store 4 (IndAddr 3)
    ,Compute Incr 2 0 2
    ,Load (IndAddr 2) 3
    ,Compute Lt 3 0 6
    ,Branch 6 (Rel 10)
    ,Compute Add 3 0 6
    ,TestAndSet (IndAddr 6)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-4))
    ,ComputeI Add 3 1 3
    ,WriteInstr 4 (IndAddr 3)
    ,ComputeI Sub 3 1 3
    ,WriteInstr 0 (IndAddr 3)
    ,Compute Incr 5 0 5
    ,ComputeI Add 2 2 2
    ,Jump (Rel (-23))
    ,Compute Decr 7 0 2
    ,Load (IndAddr 2) 6
    ,Load (IndAddr 7) 7
    ,Jump (Ind 6)
    ,Load (ImmValue 1) 2
    ,Compute Sub 7 2 2
    ,Load (ImmValue 1) 5
    ,ComputeI Gt 5 0 6
    ,Branch 6 (Rel 7)
    ,Load (IndAddr 2) 3
    ,Compute Add 7 5 6
    ,Store 3 (IndAddr 6)
    ,Compute Incr 5 0 5
    ,ComputeI Add 2 3 2
    ,Jump (Rel (-7))
    ,Compute Add 7 0 4
    ,ComputeI Add 4 1 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (ImmValue 1) 6
    ,Push 6
    ,Load (ImmValue 35) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 36) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Load (ImmValue 39) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 40) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 33) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 34) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 39) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 40) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 3
    ,Pop 2
    ,Compute Equal 2 3 4
    ,Push 4
    ,Pop 3
    ,Pop 2
    ,Compute And 2 3 4
    ,Push 4
    ,Pop 6
    ,ComputeI Xor 6 1 6
    ,Branch 6 (Rel 7)
    ,Compute Add 7 0 4
    ,ComputeI Add 4 1 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (IndAddr 7) 7
    ,Jump (Rel (-38))
    ,Load (ImmValue 5) 6
    ,Push 6
    ,Compute Add 7 0 6
    ,ComputeI Add 6 1 6
    ,Pop 5
    ,Store 5 (IndAddr 6)
    ,Compute Add 7 0 6
    ,ComputeI Add 6 1 6
    ,Load (IndAddr 6) 5
    ,Push 5
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 3
    ,Pop 2
    ,Compute Gt 2 3 4
    ,Push 4
    ,Pop 6
    ,ComputeI Xor 6 1 6
    ,Branch 6 (Rel 43)
    ,Compute Add 7 0 4
    ,ComputeI Add 4 2 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (ImmValue 37) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 38) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Pop 2
    ,Compute Decr 2 0 4
    ,Push 4
    ,Load (ImmValue 37) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 38) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Compute Add 7 0 6
    ,Load (IndAddr 6) 6
    ,ComputeI Add 6 1 6
    ,Load (IndAddr 6) 5
    ,Push 5
    ,Pop 2
    ,Compute Decr 2 0 4
    ,Push 4
    ,Compute Add 7 0 6
    ,Load (IndAddr 6) 6
    ,ComputeI Add 6 1 6
    ,Pop 2
    ,Store 2 (IndAddr 6)
    ,Push 2
    ,Load (IndAddr 7) 7
    ,Jump (Rel (-54))
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Load (ImmValue 35) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 36) 4
    ,Pop 6
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (IndAddr 7) 7
    ,Load (ImmValue 0) 2
    ,Compute Sub 7 2 2
    ,ComputeI Add 0 1 5
    ,ComputeI Gt 5 0 6
    ,Branch 6 (Rel 23)
    ,Compute Add 7 5 6
    ,Load (IndAddr 6) 4
    ,Load (IndAddr 2) 3
    ,Compute Lt 3 0 6
    ,Branch 6 (Rel 2)
    ,Store 4 (IndAddr 3)
    ,Compute Incr 2 0 2
    ,Load (IndAddr 2) 3
    ,Compute Lt 3 0 6
    ,Branch 6 (Rel 10)
    ,Compute Add 3 0 6
    ,TestAndSet (IndAddr 6)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-4))
    ,ComputeI Add 3 1 3
    ,WriteInstr 4 (IndAddr 3)
    ,ComputeI Sub 3 1 3
    ,WriteInstr 0 (IndAddr 3)
    ,Compute Incr 5 0 5
    ,ComputeI Add 2 2 2
    ,Jump (Rel (-23))
    ,Compute Decr 7 0 2
    ,Load (IndAddr 2) 6
    ,Load (IndAddr 7) 7
    ,Jump (Ind 6)
    ,Load (ImmValue 1) 2
    ,Compute Sub 7 2 2
    ,Load (ImmValue 1) 5
    ,ComputeI Gt 5 0 6
    ,Branch 6 (Rel 7)
    ,Load (IndAddr 2) 3
    ,Compute Add 7 5 6
    ,Store 3 (IndAddr 6)
    ,Compute Incr 5 0 5
    ,ComputeI Add 2 3 2
    ,Jump (Rel (-7))
    ,Compute Add 7 0 4
    ,ComputeI Add 4 1 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,TestAndSet (DirAddr 1)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-3))
    ,Load (ImmValue 5) 4
    ,Load (ImmValue 0) 5
    ,WriteInstr 5 (DirAddr 4)
    ,Load (ImmValue 57) 6
    ,Push 6
    ,Pop 5
    ,WriteInstr 5 (DirAddr 3)
    ,WriteInstr 0 (DirAddr 2)
    ,Load (ImmValue 1) 3
    ,ReadInstr (IndAddr 3)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-3))
    ,TestAndSet (DirAddr 1)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-3))
    ,Load (ImmValue 5) 4
    ,Load (ImmValue 0) 5
    ,WriteInstr 5 (DirAddr 4)
    ,Load (ImmValue 237) 6
    ,Push 6
    ,Pop 5
    ,WriteInstr 5 (DirAddr 3)
    ,WriteInstr 0 (DirAddr 2)
    ,Load (ImmValue 1) 3
    ,ReadInstr (IndAddr 3)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-3))
    ,Compute Equal 0 1 6
    ,Branch 6 (Rel 3)
    ,Load (ImmValue 2) 2
    ,EndProg
    ,Load (ImmValue 30) 3
    ,Load (ImmValue 0) 2
    ,ReadInstr (IndAddr 3)
    ,Receive 4
    ,Compute Add 2 4 2
    ,ComputeI NEq 3 33 6
    ,Compute Incr 3 0 3
    ,Branch 6 (Rel (-5))
    ,Compute Equal 2 0 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-10))
    ,Load (ImmValue 37) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-4))
    ,Load (ImmValue 38) 4
    ,ReadInstr (IndAddr 4)
    ,Receive 5
    ,Push 5
    ,WriteInstr 0 (IndAddr 2)
    ,Pop 6
    ,WriteInstr 6 (DirAddr 65536)
    ,Load (IndAddr 7) 7
    ,Load (ImmValue 0) 2
    ,Compute Sub 7 2 2
    ,ComputeI Add 0 1 5
    ,ComputeI Gt 5 0 6
    ,Branch 6 (Rel 23)
    ,Compute Add 7 5 6
    ,Load (IndAddr 6) 4
    ,Load (IndAddr 2) 3
    ,Compute Lt 3 0 6
    ,Branch 6 (Rel 2)
    ,Store 4 (IndAddr 3)
    ,Compute Incr 2 0 2
    ,Load (IndAddr 2) 3
    ,Compute Lt 3 0 6
    ,Branch 6 (Rel 10)
    ,Compute Add 3 0 6
    ,TestAndSet (IndAddr 6)
    ,Receive 6
    ,Branch 6 (Rel 2)
    ,Jump (Rel (-4))
    ,ComputeI Add 3 1 3
    ,WriteInstr 4 (IndAddr 3)
    ,ComputeI Sub 3 1 3
    ,WriteInstr 0 (IndAddr 3)
    ,Compute Incr 5 0 5
    ,ComputeI Add 2 2 2
    ,Jump (Rel (-23))
    ,Compute Decr 7 0 2
    ,Load (IndAddr 2) 6
    ,Load (IndAddr 7) 7
    ,Jump (Ind 6)
    ,Nop
    ,Nop
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 6
    ,Load (ImmValue 33) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-3))
    ,Load (ImmValue 34) 4
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 6
    ,Load (ImmValue 35) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-3))
    ,Load (ImmValue 36) 4
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 6
    ,Load (ImmValue 39) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-3))
    ,Load (ImmValue 40) 4
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Load (ImmValue 0) 6
    ,Push 6
    ,Pop 6
    ,Load (ImmValue 37) 2
    ,TestAndSet (IndAddr 2)
    ,Receive 3
    ,Branch 3 (Rel 2)
    ,Jump (Rel (-3))
    ,Load (ImmValue 38) 4
    ,WriteInstr 6 (IndAddr 4)
    ,WriteInstr 0 (IndAddr 2)
    ,Compute Add 7 0 4
    ,ComputeI Add 4 1 4
    ,Load (ImmValue 0) 5
    ,Load (ImmValue 586) 6
    ,Push 6
    ,Pop 5
    ,Store 5 (IndAddr 4)
    ,Compute Incr 4 0 4
    ,Store 7 (IndAddr 4)
    ,Compute Add 4 0 7
    ,Load (ImmValue 417) 6
    ,Push 6
    ,Pop 2
    ,Jump (Ind 2)
    ,Load (ImmValue 1) 2
    ,WriteInstr 2 (DirAddr 0)
    ,EndProg]
