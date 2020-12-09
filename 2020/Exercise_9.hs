module Exercise9 where

import Control.Monad(guard)
import Data.Functor((<&>))
import Data.List(tails, inits)
import Util

-- isSumOf :: Int -> [Int] ->
-- given a target and a list of elements, is target sum of two distinct elems in List? 
isSumOfPair :: (Eq a, Num a) => a -> [a] -> Bool
isSumOfPair target = (not . null) . findSums target

{- This is a very brute force solution to problem 1
- A more performant solution is to fold with a state,
- where the state has the list 
-}
findSums :: (Eq a, Num a) => a -> [a] -> [(a, a)]
findSums target xs = 
  do (x: ts) <- filter (longerThan 2) . tails $ xs
     y <- ts
     guard $ x + y == target
     pure (x, y)

segmentsOfLength :: Int -> [a] -> [[a]]
segmentsOfLength len = map (take len) . filter (longerThan len) . tails

problem1 :: (Eq a, Num a) => Int -> [a] -> a
problem1 previous =
  fst . head
  . filter (not . uncurry (isSumOfPair))
  . map (\xs -> (last xs, init xs))
  . segmentsOfLength (previous + 1)
  

testInput1 = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
testProblem1 = problem1 5 testInput1
testProblem2 = problem2 5 testInput1 == 62

findContiguousSum :: (Eq a, Num a) => a -> [a] -> [[a]]
findContiguousSum target = 
  filter ( (== target) . sum ) . filter (not . null) . (=<<) inits . tails

problem2 :: (Num a, Ord a) => Int -> [a] -> a
problem2 len xs = secret . head . findContiguousSum (problem1 len xs) $ xs
  where secret ys = maximum ys + minimum ys

parseInput :: IO [Int]
parseInput = readFile "input9.txt" <&> ( map read . filter (not . null) . lines)
