module IntCode where

import Util
import Control.Monad.State

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

data IntVM = IntVM { progCounter :: Int, memory :: [Int] , inps :: [Int] , outps :: [Int] }

evalParam :: Monad m => Param -> StateT IntVM m Int
evalParam param = case param of
  Immediate val -> pure val
  Position  pos -> gets ((!! pos) . memory)

store :: Monad m => Int -> Int -> StateT IntVM m ()
store pos val = modify (\st -> st { memory = setAt pos val (memory st) } )

continueAt :: Monad m => (Int -> Int) -> StateT IntVM m ()
continueAt f = modify (\st -> st { progCounter = f (progCounter st) })

enqueueInput :: Monad m => Int -> StateT IntVM m ()
enqueueInput i = modify $ \st -> st { inps = inps st ++ [i] }

popInput :: Monad m => StateT IntVM m Int
popInput = do
  st @ (IntVM _ _ (i:is) _) <- get
  put $ st { inps = is }
  pure i

popOutput :: Monad m => StateT IntVM m Int
popOutput = do
  st @ (IntVM _ _ _ (o:os)) <- get
  put $ st { outps = os }
  pure o

pushOutput :: Monad m => Int -> StateT IntVM m ()
pushOutput o = do
  st @ (IntVM _ _ _ os) <- get
  put $ st { outps = o:os }

noOutput :: Monad m => StateT IntVM m Bool
noOutput = gets (null . outps)

fetch :: Monad m => StateT IntVM m Instruction
fetch = gets $ \st  -> instruction $ drop (progCounter st) (memory st)

execInst :: Monad m => Instruction -> StateT IntVM m ()
execInst (Instruction inst params) = case (inst, params) of
  (Halt, _) ->
    lift $ fail "IntCode program finished"
  (Inp, [Position res]) -> do
    store res =<< popInput
    continueAt (2 +)
  (OutP,  [param]) -> do
    pushOutput =<< evalParam param
    continueAt (2 +)
  (JumpIf b, [pcond, pjump]) ->
    let decide cond jump = if sw cond then const jump else (3 +)
        sw = (if b then not else id) . (== 0)
    in
      continueAt =<< (decide <$> evalParam pcond <*> evalParam pjump)
  (Oper arith, [p1, p2, Position pos]) -> do
    store pos =<< (execArith arith <$> evalParam p1 <*> evalParam p2)
    continueAt (4 +)

--- Step: Just if next state, Nothing is finished.
execStep :: Monad m => IntVM -> m IntVM
execStep = execStateT (fetch >>= execInst)

-- evalProgram: takes program and inputs, leads outputs.
evalProgram :: Memo -> [Int] -> [Int]
evalProgram memo ins = outps . last . unfoldMb execStep $ IntVM 0 memo ins []
