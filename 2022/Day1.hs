module Exercise1 where

import Data.Functor((<&>))
import Data.List(span, drop)

{--
> [...] One important consideration is food - in particular,
> the number of Calories each Elf is carrying (your puzzle input)

> Each Elf separates their own inventory from the previous Elf's inventory (if any) by a blank line.

--}

splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks xs =
  let (pre, post) = span (not . null) xs
  in  pre : splitBlocks (drop 1 post)

addBlock :: [String] -> Int
addBlock = sum . map read

-- BackPacks: split input into list of totals, one item per elf. 
backPacks :: String -> [Int]
backPacks = map addBlock . splitBlocks . lines

{--
> they'd like to know how many Calories are being carried by the Elf carrying the most Calories
--}
part1 :: String -> Int
part1 = maximum . backpacks

{--
> the Elves would like to know the total Calories carried by the top three Elves carrying the most Calories
--}
part2 :: String -> Int
part2 = sum . foldl keep3 [] . backpacks where
  -- insert: ordered insertion 
  ordInsert :: Ord a => a -> [a]  -> [a]
  ordInsert x [] = [x]
  ordInsert x (y: ys)
    | x >= y   = x : y : ys
    | x < y    = y : (ordInsert x ys)

  keep3 :: [Int] -> Int -> [Int]
  keep3 xs x = take 3 $ ordInsert x xs

solveProblem :: IO Int
solveProblem = readFile "input1.txt" <&> part2
