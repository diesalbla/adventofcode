module IntCode where

import Util 

data Arith =  Add | Mult | LessThan | EqualTo deriving (Eq, Show)

data OpCode = Oper Arith | JumpIf Bool | Inp | OutP | Halt deriving (Eq, Show)

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

execArith :: Arith -> (Int -> Int -> Int)
execArith arith = case arith of 
  Add -> (+)
  Mult -> (*) 
  LessThan -> (fromEnum .) . (<)
  EqualTo ->  (fromEnum .) . (==)

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

evalParam :: [Int] -> Param -> Int
evalParam mem (Position n ) = mem !! n
evalParam _   (Immediate n) = n

data State = State { progCounter :: Int, memo :: [Int] , inps :: [Int] , outps :: [Int] }

execInst :: Instruction -> State -> Maybe State
execInst (Instruction Halt _ ) _ = Nothing
execInst (Instruction opc params) (State pc mem ins outs) = Just $ case opc of 
  Inp -> State nextPc (setAt res (head ins) mem) (tail ins) outs  where
    [Position res] = params
  OutP -> State nextPc mem ins  (out: outs)  where
    [out] = map (evalParam mem) params
  JumpIf b -> State npc mem ins outs where
    [pcond, pjump] = map (evalParam mem) params
    jumps = (if b then not else id) . (== 0) $ pcond
    npc = if jumps then pjump else nextPc
  Oper arith -> State nextPc nmem ins outs  where
    nmem = setAt res nval mem
    [p1, p2, Position res] = params
    nval = execArith arith (evalParam mem p1) (evalParam mem p2)
  where
    nextPc = pc + 1 + length params

--- Step: Just if next state, Nothing is finished. 
execStep :: State -> Maybe State
execStep st@(State pc mem _ _) = execInst (instruction (drop pc mem)) st
          
-- evalProgram: takes program and inputs, leads outputs. 
evalProgram :: Memo -> [Int] -> [Int]
evalProgram memo ins = outps . last . unfoldMb execStep $ State 0 memo ins []
