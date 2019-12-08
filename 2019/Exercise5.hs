module Exercise5 where

import Util 

data Arith =  Add | Mult | LessThan | EqualTo deriving (Eq, Show)

data OpCode = Oper Arith
          | JumpIf Bool
          | Inp
          | OutP
          | Halt
  deriving (Eq, Show)

data Param = Position Int | Immediate Int deriving (Eq, Show)

params :: Int -> [Int -> Param]
params = map fun . digits where 
  fun  m   = if m == 0 then Position else Immediate
  digits x = x `mod` 10  : digits (x `quot` 10)

data Instruction = Instruction OpCode [Param] deriving (Eq, Show)

numCodes :: OpCode -> Int
numCodes op = case op of
  Halt -> 0
  JumpIf _ -> 2
  Oper  _  -> 3
  Inp -> 1
  OutP -> 1

-- replace ix x xs:  item at postition ix by t
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 y (_:xs) = y:xs
setAt i y (x:xs) = x : setAt (i-1) y xs

-- parseInstruction is assumed to be at head , so non-empty list 
instruction :: [Int] -> Instruction
instruction (ncode:memo) = Instruction opcode ps where
  opcode = case ncode `mod` 100 of 
    1  -> Oper Add
    2  -> Oper Mult
    3  -> Inp
    4  -> OutP
    5  -> JumpIf True
    6  -> JumpIf False
    7  -> Oper LessThan
    8  -> Oper EqualTo
    99 -> Halt
  ps     = take (numCodes opcode) $ zipWith ($) (params (ncode `quot` 100)) memo

type Memo  = [Int]

data State = State { progCounter :: Int, memo :: [Int] , outs :: [Int] }

execInst :: Instruction -> State -> Maybe State
execInst (Instruction Halt _ ) _ = Nothing
execInst (Instruction opc params) (State pc mem outs) = Just $ case opc of 
  Inp -> State nextPc (setAt res 5 mem) outs  where
    [Position res] = params
  OutP -> State nextPc mem (out: outs)  where
    [out] = map (evalParam mem) params
  JumpIf b -> State npc mem outs where
    [pcond, pjump] = map (evalParam mem) params
    jumps = (if b then not else id) . (== 0) $ pcond
    npc = if jumps then pjump else nextPc
  Oper arith -> State nextPc nmem outs  where
    nmem = setAt res nval mem
    [p1, p2, Position res] = params
    nval = evalParam mem p1 `fun` evalParam mem p2
    fun = case arith of 
      Add -> (+)
      Mult -> (*) 
      LessThan -> (fromEnum .) . (<)
      EqualTo ->  (fromEnum .) . (==)
  where
    nextPc = pc + 1 + length params

    evalParam :: [Int] -> Param -> Int
    evalParam mem (Position n ) = mem !! n
    evalParam _   (Immediate n) = n

--- Step: Just if next state, Nothing is finished. 
step :: State -> Maybe State
step st@(State pc mem _) = execInst (instruction (drop pc mem)) st
          
runProgram :: Memo -> Int
runProgram = head . outs . last . unfoldMb step . initState where 
  initState memo = State 0 memo []

test1 :: Memo
test1 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

input1 :: Memo
input1 = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,89,49,225,1102,35,88,224,101,-3080,224,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1101,25,33,224,1001,224,-58,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1102,78,23,225,1,165,169,224,101,-80,224,224,4,224,102,8,223,223,101,7,224,224,1,224,223,223,101,55,173,224,1001,224,-65,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,2,161,14,224,101,-3528,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1002,61,54,224,1001,224,-4212,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1101,14,71,225,1101,85,17,225,1102,72,50,225,1102,9,69,225,1102,71,53,225,1101,10,27,225,1001,158,34,224,101,-51,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,102,9,154,224,101,-639,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,226,224,102,2,223,223,1006,224,329,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,344,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,359,1001,223,1,223,108,226,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,107,226,677,224,102,2,223,223,1006,224,389,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,404,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1007,226,226,224,102,2,223,223,1006,224,434,1001,223,1,223,1108,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,479,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,494,101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,509,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,524,101,1,223,223,7,226,226,224,102,2,223,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,584,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,614,101,1,223,223,108,677,677,224,102,2,223,223,1005,224,629,1001,223,1,223,8,677,677,224,1002,223,2,223,1005,224,644,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,659,1001,223,1,223,1007,226,677,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226]
