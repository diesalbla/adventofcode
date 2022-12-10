
module Exercise6 where

import Data.List (nub, find, tails)
import Data.Functor((<&>))

findMark :: Eq a => Int -> [a] -> Int
findMark num = 
  maybe 0 fst
  . find ((== num) . length . nub . snd)
  . zip [num .. ]
  . map (take num)
  . tails


solveProblem = readFile "input6.txt" <&> findMark 14
