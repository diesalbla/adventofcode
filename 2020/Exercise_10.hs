module Exercise10 where

import Data.Functor((<&>))
import Data.List(sort, group, lookup)
import Data.Maybe(fromMaybe)
  
parseInput :: IO [Int]
parseInput = readFile "input10.txt" <&> ( map read . filter (not . null) . lines)

{- Notes on the sorted Jumps:
   - there is an implicit initial 0 voltage, which is our outlet.
   - there is a final 3 at the end, which is our device adapter.
-}
sortedJumps :: [Int] -> [Int]
sortedJumps jolts = 
  let srt = sort (0 : jolts)
  in (zipWith (-) (tail srt) srt) ++ [3]

problem1 :: [Int] -> Int
problem1 jolts =
  let grus = group . sort . sortedJumps $ jolts
      summary = map (\xs -> (head xs, length xs)) grus
      answer = do ones <- lookup 1 summary
                  thrs <- lookup 3 summary
                  pure $ ones * thrs
  in fromMaybe 0 answer  

--
-- counting arrangements can be done from the list of differences on the sorted list.
-- whenever we have a sequence (1, 1) representing (x -1- y -1- z) we can skip 1,
-- and thus turn it into (2:)
--
-- This is the multiple-recursive exponential time solution, bad!
-- 
chains :: [Int] -> Int
chains [] = 1
chains [_] = 1
chains (x:y:ys) = chains (y:ys) + (if x +  y <= 3 then chains (x+y:ys) else 0)

problem2 :: [Int] -> Int
problem2 = chains . sortedJumps


{- Explanation: the combinatoric problem is that we can skip elements
in list of differences, but if we make that choice then jump increases,
so we need to count that when moving to next element.

The state is a table indicating, for each possible last jump of the
processed list, how many ways there are to end with it.

Note: since we only have at most 4 entries (0, 1, 2, 3), we could just skip to a tuple.
or we could start not with zero, but taking the head.
-}
type State = [(Int, Int)]

initState = [(0, 1)] -- there is 1 way to get a chain ending on 0 

stepState :: State -> Int -> State
stepState st x = (x, sumPrevs st) : merges st where
  sumPrevs = sum . map snd
  merges  = filter ( (<= 3) . fst) . map ( \ (k, v) -> (k+x, v)) . filter ( (> 0) . fst)

problem2b = foldl stepState initState . sortedJumps

testInput2 :: [Int]
testInput2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

