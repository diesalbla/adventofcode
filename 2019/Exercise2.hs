module Exercise2 where

import qualified Data.Maybe as Mb

type Memo  = [Int]
type State = (Int, Memo)

-- replace ix x xs:  item at postition ix by t
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 y (_:xs) = y:xs
setAt i y (x:xs) = x : setAt (i-1) y xs

--- Step: Just if next state, Nothing is finished. 
step :: State -> Maybe State
step (pc, mem) = if head tetra == 99 then Nothing else Just (pc+1, nmem) where
  tetra = take 4 (drop (4*pc) mem)
  nmem  = case tetra of
    [op,a1,a2,res] -> setAt res nval mem where
      nval = (oper op) (mem !! a1) (mem !! a2)
      oper 1 = (+)
      oper 2 = (*)

compute :: State -> State
compute = last . Mb.catMaybes . takeWhile Mb.isJust . iterate (>>= step) . Just

runProgram :: Int -> Int -> Memo -> Int
runProgram name verb =
  head . snd . compute . ((,) 0) . setAt 1 name . setAt 2 verb

findValues:: Int
findValues = head $ do
  x <- [0 .. 99]
  y <- [0 .. 99]
  if runProgram x y input1 == 19690720 then [x * 100 + y] else [] 

input1 :: Memo
input1 = undefined -- your personalised input here
