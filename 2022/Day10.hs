
module Day10 where

import Data.Functor((<&>))


data Instr = Noop | AddX Int

parseInstr :: String -> Instr
parseInstr "noop" = Noop
parseInstr ('a': 'd': 'd': 'x': ' ': num) = AddX (read num)

type State = Int

initState = 1

runInstr :: Instr -> State -> [State]
runInstr Noop st = [st] -- takes one cycle
runInstr (AddX n) st = [st, st + n] 

runProgram :: State -> [Instr] -> [State]
runProgram st [] = []
runProgram st (i:is) =
  let pre = runInstr i st
  in  pre ++ runProgram (last pre) is


testProgram = [Noop, AddX 3, AddX (-5)]


problem1 = sum . map (uncurry (*)) . (\xs -> map (xs !!) samples) . zip [0..] . (initState :) . runProgram initState

samples = [20, 60, 100, 140, 180, 220]


solveProblem = readFile "testInput10.txt" <&> problem1 . map parseInstr . lines
