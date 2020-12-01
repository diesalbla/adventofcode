module Exercise1 where

import Control.Monad(guard)
import qualified Data.Set as S
import qualified Data.List as L

{--
> Specifically, they need you to find the two entries that sum to 2020
> and then multiply those two numbers together.
--}

problemA :: Int -> [Int] -> Int
problemA goal = head . findSumA goal

findSumA :: (Eq a, Num a) => a -> [a] -> [a]
findSumA goal xs = 
  do (x:ys) <- filter (longerThan 2) (L.tails xs)
     y <- ys
     guard $ x+y == goal
     pure $ x * y

improvedA :: (Ord a, Num a) => a -> [a] -> [a]
improvedA goal xs =
  do x <- xs
     guard $ S.member (goal - x) allNums
     pure $ x * (goal - x)
  where allNums = S.fromList xs  

{-
> They offer you a second one if you can find three numbers
> in your expense report that meet the same criteria.
-}
problemB :: Int -> [Int] -> Int
problemB goal = head . findSumB goal

findSumB :: (Eq a, Num a) => a -> [a] -> [a]
findSumB goal xs = 
  do (x:ys) <- takeWhile (longerThan 3) (L.tails xs)
     (y:zs) <- takeWhile (longerThan 2) (L.tails ys)
     z <- zs
     guard $ x+y+z == goal
     pure $ x * y * z

improvedB :: (Ord a, Num a) => a -> [a] -> [a]
improvedB goal xs = 
  do (x:ys) <- takeWhile (longerThan 2) (L.tails xs)
     y <- ys
     let z = goal - x - y
     guard $ S.member z allNums
     pure $ x * y * z
  where n = length xs
        allNums = S.fromList xs

-- FUTURE WORK: use saddleback search (Richard Bird Pearls) for this problem,
-- both in the 2D and the 3D variants.

testInput = [1721, 979, 366 , 299, 675, 1456]

test1 = problemA 2020 testInput == 514579
test2 = problemB 2020 testInput == 241861950
test3 = head (improvedA 2020 testInput) == 514579
test4 = head (improvedB 2020 testInput) == 241861950


input1 :: [Int]
input1 = undefined

longerThan :: Int -> [a] -> Bool
longerThan m = (== m) . length . take m
