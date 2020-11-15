module Exercise2 where

import Util(unfoldMb)
import Control.Monad(guard)

type Memo  = [Int]

data State = State { pc :: Int, memo :: Memo }

-- replace ix x xs:  item at postition ix by t
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 y (_:xs) = y:xs
setAt i y (x:xs) = x : setAt (i-1) y xs

--- Step: Just if next state, Nothing is finished. 
step :: State -> Maybe State
step (State pc mem) = if head tetra == 99 then Nothing else Just (State (pc+1) nmem) where
  tetra = take 4 (drop (4*pc) mem)
  nmem  = case tetra of
    [op,a1,a2,res] -> setAt res nval mem where
      nval = (oper op) (mem !! a1) (mem !! a2)
      oper 1 = (+)
      oper 2 = (*)

runToHalt:: State -> State
runToHalt = last . unfoldMb step
  
{-
- Once the program has halted, its output is available at address 0 [...]
-}
getOutput :: State -> Int
getOutput = head . memo

{-
Once the program has halted, its output is available at address 0 [...] .
Each time you try a pair of inputs [...], make sure you first reset the computer's
memory to the values in the program (your puzzle input) [...]
-}
runProgram :: Int -> Int -> Memo -> Int
runProgram name verb = getOutput . runToHalt . initState name verb

initState :: Int -> Int -> Memo -> State
initState name verb memo = State 0 (setAt 1 name . setAt 2 verb $ memo)

{-
Find the input noun and verb that cause the program to produce the output 19690720.
What is 100 * noun + verb? (
-}
findInputs :: Int -> [Int]
findInputs desired = do
  x <- [0 .. 99]
  y <- [0 .. 99]
  guard $ runProgram x y input1 == desired
  pure $ x * 100 + y

part2 = head $ findInputs 19690720

  
input1 :: Memo
input1 = undefined

