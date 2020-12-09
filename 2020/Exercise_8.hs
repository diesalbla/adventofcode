
module Exercise8 where

import qualified Data.Maybe as Mb
import Data.Functor((<&>))
import qualified Data.Set as S
import Util

{-
The boot code is represented as a text file with one instruction per line of text.
Each instruction consists of an operation (acc, jmp, or nop)
and an argument (a signed number like +4 or -20).
-}

unfoldMb :: (a -> Maybe a) -> a -> [a]
unfoldMb f = Mb.catMaybes . takeWhile Mb.isJust . iterate (>>= f) . Just


data Inst = Acc Int
          | Jmp Int
          | Nop Int
  deriving (Eq, Ord, Show)


parseInst :: String -> Inst
parseInst str =
  let [cod, num] = splitAtElem ' ' str
      parseNum :: String -> Int
      parseNum = read . filter (/= '+')
  in case cod of
       "acc" -> Acc (parseNum num)
       "jmp" -> Jmp (parseNum num)
       "nop" -> Nop (parseNum num)

testProgram = [ "nop +0"
              , "acc +1"
              , "jmp +4"
              , "acc +3"
              , "jmp -3"
              , "acc -99"
              , "acc +1"
              , "jmp -4"
              , "acc +6"
              ]

data Compute = Compute { program :: [Inst]
                       , progCounter :: Int
                       , accumulator :: Int
                       }
               deriving (Eq, Ord, Show)

runStep :: Compute -> Maybe Compute
runStep (Compute prog pc acc) =
  if pc >= length prog
  then Nothing
  else Just $ case prog !! pc of
                Acc val -> Compute prog (pc + 1) (acc + val)
                Jmp off -> Compute prog (pc + off) acc
                Nop _   -> Compute prog (pc + 1) acc
  
traceProgram :: [Inst] -> [Compute]
traceProgram = unfoldMb runStep . initState where
  initState prog = Compute prog 0 0

{- collectSemantics !! i == all positions visited before state i
-}
collectSemantics :: [Compute] -> [S.Set Int]
collectSemantics = scanl (flip S.insert) S.empty . map progCounter

{-
Immediately before any instruction is executed a second time,
what value is in the accumulator?
-}
part1 :: [Inst] -> Int
part1 prog = accumulator . fst . last . takeWhile (not . (uncurry hasLooped)) $ zipped
  where
  trace      = traceProgram prog
  preVisited = collectSemantics trace
  zipped     = zip trace preVisited

hasLooped :: Compute -> S.Set Int -> Bool
hasLooped cmp vis = S.member (progCounter cmp) vis

parseInput :: IO [Inst]
parseInput = readFile "input8.txt" <&> ( map parseInst . filter (not . null) . lines)

{-
Fix the program so that it terminates normally by changing
exactly one jmp (to nop) or nop (to jmp).
What is the value of the accumulator after the program terminates?
-}
hasLoop :: [Inst] -> Bool
hasLoop prog = not . null . filter (uncurry hasLooped) $ zip trace preVisited
  where trace      = traceProgram prog
        preVisited = collectSemantics trace

mutations :: [Inst] -> [[Inst]]
mutations [] = []
mutations (inst: insts) = mutateHead ++ mutatedTails
  where mutatedTails =  map (inst :) (mutations insts)
        mutateHead = case inst of
          Acc v -> []
          Jmp v -> [Nop v: insts]
          Nop v -> [Jmp v: insts]

part2 :: [Inst] -> Int
part2 = accumulator . last . traceProgram . head . filter (not . hasLoop) . mutations
