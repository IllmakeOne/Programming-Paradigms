{-# LANGUAGE RecordWildCards #-}

module Sprockell.Sprockell where

import Data.Bits

import Sprockell.BasicFunctions
import Sprockell.HardwareTypes

{-------------------------------------------------------------
| SPROCKELL: Simple PROCessor in hasKELL :-)
|
| Initial definition: October 2012, Jan Kuper (j.kuper@utwente.nl)
| Extensions: June 2015, Martijn Bastiaan, Arjan Boeijink, Jan Kuper, Leon Schoorl
| Simplification: January 2016, Jan Kuper
| Further extensions: June 2017, Leon Schoorl
-------------------------------------------------------------}

-- =====================================================================================
-- sprockell: combines the separate components defined below
--      instrs  : list of instructions to be executed
--      sprState: state of the sprockell, containing pc, sp, registers, local memory
--      reply   : input from shared memory (Maybe type)
--      request : output to shared memory
-- =====================================================================================

sprockell :: InstructionMem -> SprockellState -> Reply -> (SprockellState, Request)

sprockell instrs sprState reply = (sprState', request)
    where
        SprState{..} = sprState
        MachCode{..} = decode (instrs!pc)

        regbankExtended = regbank ++ [sp,pc] -- allow reading of sp and pc

        (x,y)        = (regbankExtended ! regX , regbankExtended ! regY)
        aluOutput    = alu aluCode x y
        aluIOutput   = alu aluCode x immValue

        pc'          = nextPC branch tgtCode (x,reply) (pc,immValue,y)

        sp'          | loadReg == regSP = loadValue
                     | otherwise        = nextSP spCode sp

        address      = agu aguCode (addrImm,x,sp)

        loadValue    = load ldCode (immValue, aluOutput, aluIOutput, localMem!address, reply)
        regbank'     = regbank <~! (loadReg, loadValue)

        localMem'    = store localMem stCode (address,y)

        sprState'    = SprState {pc=pc', sp=sp', regbank=regbank', localMem=localMem'}

        request      = sendOut ioCode address y



-- =====================================================================================
-- decode function + default machine code (nullcode): to generate machine code from an instruction
-- =====================================================================================

nullcode :: MachCode

nullcode = MachCode
        { ldCode   = LdImm
        , stCode   = StNone
        , aguCode  = AguDir
        , branch   = BNS
        , tgtCode  = NoJump
        , spCode   = Flat
        , aluCode  = Or
        , ioCode   = IONone
        , immValue = 0
        , regX     = 0
        , regY     = 0
        , loadReg  = 0
        , addrImm  = 0
        }

-- ============================

decode :: Instruction -> MachCode
decode instr = case instr of

  Compute c rx ry toReg       -> nullcode {ldCode=LdAlu, aluCode=c, regX=rx, regY=ry, loadReg=toReg}
  ComputeI c rx vy toReg      -> nullcode {ldCode=LdAluI, aluCode=c, regX=rx, regY=vy, immValue=vy, loadReg=toReg}

  Jump target                 -> case target of
                                   Abs n       -> nullcode {tgtCode=TAbs, immValue=n}
                                   Rel n       -> nullcode {tgtCode=TRel, immValue=n}
                                   Ind r       -> nullcode {tgtCode=TInd, regY=r}

  Branch cReg target          -> case target of
                                   Abs n       -> nullcode {branch=BN, tgtCode=TAbs, regX=cReg, immValue=n}
                                   Rel n       -> nullcode {branch=BN, tgtCode=TRel, regX=cReg, immValue=n}
                                   Ind r       -> nullcode {branch=BN, tgtCode=TInd, regX=cReg, regY=r}

  BranchX cReg target          -> case target of
                                   Abs n       -> nullcode {branch=BX, tgtCode=TAbs, regX=cReg, immValue=n}
                                   Rel n       -> nullcode {branch=BX, tgtCode=TRel, regX=cReg, immValue=n}
                                   Ind r       -> nullcode {branch=BX, tgtCode=TInd, regX=cReg, regY=r}

  Load memAddr toReg          -> case memAddr of
                                   ImmValue n  -> nullcode {loadReg=toReg, ldCode=LdImm, immValue=n}
                                   DirAddr a   -> nullcode {loadReg=toReg, ldCode=LdMem, aguCode=AguDir, addrImm=a}
                                   IndAddr p   -> nullcode {loadReg=toReg, ldCode=LdMem, aguCode=AguInd, regX=p}

  Store fromReg memAddr       -> case memAddr of
                                   ImmValue n  -> nullcode -- Undefined. Should not occur.
                                   DirAddr a   -> nullcode {stCode=StMem, regY=fromReg, ldCode=LdMem, aguCode=AguDir, addrImm=a}
                                   IndAddr p   -> nullcode {stCode=StMem, regY=fromReg, ldCode=LdMem, aguCode=AguInd, regX=p}

  Push fromReg                -> nullcode {stCode=StMem, regY=fromReg, aguCode=AguPush, spCode=Down}

  Pop toReg                   -> nullcode {ldCode=LdMem, loadReg=toReg, aguCode=AguPop, spCode=Up}

  Receive toReg               -> nullcode {ldCode=LdInp, tgtCode=Waiting, loadReg=toReg}

  ReadInstr memAddr           -> case memAddr of
                                   ImmValue n  -> nullcode -- undefined
                                   DirAddr a   -> nullcode {ioCode=IORead, ldCode=LdMem, aguCode=AguDir, addrImm=a}
                                   IndAddr p   -> nullcode {ioCode=IORead, ldCode=LdMem, aguCode=AguInd, regX=p}

  WriteInstr fromReg memAddr  -> case memAddr of
                                   ImmValue n  -> nullcode -- undefined
                                   DirAddr a   -> nullcode {ioCode=IOWrite, regY=fromReg, ldCode=LdMem, aguCode=AguDir, addrImm=a}
                                   IndAddr p   -> nullcode {ioCode=IOWrite, regY=fromReg, ldCode=LdMem, aguCode=AguInd, regX=p}

  TestAndSet memAddr          -> case memAddr of
                                   ImmValue n  -> nullcode -- undefined
                                   DirAddr a   -> nullcode {ioCode=IOTest, ldCode=LdMem, aguCode=AguDir, addrImm=a}
                                   IndAddr p   -> nullcode {ioCode=IOTest, ldCode=LdMem, aguCode=AguInd, regX=p}

  EndProg                     -> nullcode {tgtCode=TRel, immValue=0}

  Nop                         -> nullcode

  Debug _                     -> nullcode       -- only for development purposes


{- ===============================================================
Meaning registers regX and regY (containing x and y, respectively)
==================================================================
                                |  regX - x     |  regY - y     |
-----------------------------------------------------------------
Compute c rx ry toReg           | 1st arg rx    | 2nd arg ry    |
LdConst n toReg                 |   -           |   -           |
Load (DirAddr a) toReg          |   -           |   -           |
Load (Defer p) toReg            | deref-reg p   |   -           |
                                |               |               |
Jump (Abs n)                    |   -           |   -           |
 -   (Rel n)                    |   -           |   -           |
 -   (Ind r)                    |   -           | ind reg r     |
Branch cReg (Abs n)             | cond-reg cReg |   -           |
 -      -   (Rel n)             | cond-reg cReg |   -           |
 -      -   (Ind r)             | cond-reg cReg | ind reg r     |
                                |               |               |
Store fromReg (DirAddr a)       |   -           | fromReg       |
 -     -      (IndAddr p)       | deref-reg p   | fromReg       |
                                |               |               |
Push fromReg                    |   -           | fromReg       |
Pop toReg                       |   -           |   -           |
                                |               |               |
Receive toReg                   |   -           |   -           |
                                |               |               |
Read (DirAddr a)                |   -           |   -           |
 -   (IndAddr p)                | deref-reg p   |   -           |
                                |               |               |
TestAndSet (DirAddr a)          |   -           |   -           |
 -         (IndAddr p)          | deref-reg p   |   -           |
                                |               |               |
Write fromReg (DirAddr a)       |   -           | fromReg       |
 -     -      (IndAddr p)       | deref-addr p  | fromReg       |
=============================================================== -}

-- =====================================================================================
-- alu (Arithmetic-Logic Unit): defines the computational functionality
-- =====================================================================================
alu :: Operator -> Value -> Value -> Value
alu op x y = case op of
        Incr   -> x + 1
        Decr   -> x - 1
        Add    -> x + y
        Sub    -> x - y
        Mul    -> x * y
        Equal  -> intBool (x == y)
        NEq    -> intBool (x /= y)
        Gt     -> intBool (x > y)
        GtE    -> intBool (x >= y)
        Lt     -> intBool (x < y)
        LtE    -> intBool (x <= y)
        And    -> x .&. y
        Or     -> x .|. y
        LShift -> shiftL x (fromIntegral y)
        RShift -> shiftR x (fromIntegral y)
        Xor    -> x `xor` y
        -- Div    -> x `div` y                          -- usable in Haskell, but expensive on hardware
        -- Mod    -> x `mod` y                          -- Ibid

-- =====================================================================================
-- agu (Address Generation Unit): calculates the address for local memory
-- =====================================================================================
agu :: AguCode -> (MemAddr,MemAddr,MemAddr) -> MemAddr
agu aguCode (addrImm,x,sp) = case aguCode of
        AguDir   -> addrImm
        AguInd   -> x
        AguPush  -> sp-1
        AguPop   -> sp

-- =====================================================================================
-- load: calculates the value that has to be put in a register
-- =====================================================================================
load :: LdCode -> (Value, Value, Value, Value, Reply) -> Value
load ldCode (immval,aluOutput,aluIOutput,memval,reply) = case (ldCode, reply) of
        (LdImm, Nothing)  -> immval
        (LdAlu, Nothing)  -> aluOutput
        (LdAluI, Nothing) -> aluIOutput
        (LdMem, Nothing)  -> memval

        (LdInp, Just rx) -> rx
        (LdInp, Nothing) -> 0

        (_    , Just rx) -> error ("Sprockell ignored a system response of value: " ++ show rx)

-- =====================================================================================
-- store: to store data in local memory
-- =====================================================================================
store :: LocalMem -> StCode -> (MemAddr, Value) -> LocalMem
store mem stCode (address,value) = case stCode of
        StNone -> mem
        StMem  -> mem <~ (address, value)

-- =====================================================================================
-- nextPC: to calculate next program counter
-- =====================================================================================
nextPC :: BranchCode -> TargetCode -> (Value,Reply) -> (Value,Value,Value) -> Value
nextPC branch tgtCode (x,reply) (pc,n,y) =

        case  (branch, tgtCode, x/=0,  reply  )  of

              ( BN , TAbs   , True,    _    )  -> n
              ( BN , TRel   , True,    _    )  -> pc + n
              ( BN , TInd   , True,    _    )  -> y

              ( BX , TAbs   , False,    _    )  -> n
              ( BX , TRel   , False,    _    )  -> pc + n
              ( BX , TInd   , False,    _    )  -> y

              ( BNS, TAbs   ,  _  ,    _    )  -> n
              ( BNS, TRel   ,  _  ,    _    )  -> pc + n
              ( BNS, TInd   ,  _  ,    _    )  -> y

              ( BNS, Waiting,  _  , Nothing )  -> pc

              (  _   ,   _    ,  _  ,    _    )  -> pc + 1

-- =====================================================================================
-- nextSP: to calculate next stack pointer
-- =====================================================================================
nextSP :: SPCode -> MemAddr -> MemAddr
nextSP spCode sp = case spCode of
        Down    -> sp-1
        Flat    -> sp
        Up      -> sp+1

-- =====================================================================================
-- sendOut: to calculate output request to shared memory
-- =====================================================================================
sendOut :: IOCode -> MemAddr -> Value -> Request
sendOut ioCode address value = case ioCode of
        IONone    -> NoRequest
        IORead    -> ReadReq  address
        IOWrite   -> WriteReq value address
        IOTest    -> TestReq  address
