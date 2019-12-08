module Exercise7 where

import IntCode
import Data.List(delete)

-- we are given a set of n instances of program memo

-- first parameter: the extra inputs
-- second parameter: the piped input-output (zero at first)
evalChain :: Memo -> [Int] -> [Int] -> [Int]
evalChain _    accs []     = accs
evalChain memo accs (x:xs) = evalChain memo (evalProgram memo (x:accs)) xs

permutations :: Int -> [[Int]]
permutations n = permutationsAux [0.. n - 1]

permutationsAux :: Eq a => [a] -> [[a]]
permutationsAux [] = [[]]
permutationsAux xs = do
  y <- xs
  let rest = delete y xs
  ys <- permutationsAux rest
  pure (y:ys)

problemA = maximum . map (head . evalChain prog1 [0]) . permutations $ 5

-- Problem 2 : very different, each program has to take 1 input, emit 1 output, and stop/wait.

-- First of all: the instruction still uses Maybe to say terminated / continue
-- but we also are going to use Either to indicate continue / pause with output.

-- so we need a different model of state, and we need to "inject" some input.
-- so inner loop: either -> either and continue on Left until a Right is found

execStepOut :: State -> Maybe (Either (State, Int) State)
execStepOut = map leftOut . execStep where
  leftOut :: State -> Either (State Int) State 
  leftOut st = case outps st of
    [] ->    Right st
    [out] -> Left (out, st { outps = [] })

-- execLeg:: continue execution until it emits one output. 
-- there must be some monad transformer to do this 
execLeg :: State -> Maybe (State, Int)
execLeg st = case execStepOut st of
  Nothing -> Nothing
  Just (Right st) -> execStepOut st
  Just (Left (st, out) ) -> Just (st, out)

-- TODO where should the output be inserted? 
execContinue :: Int -> State -> Maybe (State, Int)
execContinue inp st = execLeg (st { inps = inps ++ [inp] : inps st} )

-- how do we do the feedback ?
-- we want it to be :: MSF m a -> a  -> m [a]

--  So, here is the process...
-- first, we create the initial machines, each of them having as its first input
-- then we chain them together with `andThen` (a Fold),
-- we do the feedback loop with them... 
--


prog1 = [3,8,1001,8,10,8,105,1,0,0,21,30,55,80,101,118,199,280,361,442,99999,3,9,101,4,9,9,4,9,99,3,9,101,4,9,9,1002,9,4,9,101,4,9,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,101,5,9,9,1002,9,2,9,101,3,9,9,102,4,9,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,101,5,9,9,102,3,9,9,101,3,9,9,4,9,99,3,9,1001,9,2,9,102,4,9,9,1001,9,3,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99]       
