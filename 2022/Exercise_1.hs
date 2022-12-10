module Exercise1 where

import Data.Functor((<&>))
import Data.List(span, drop)


splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks xs =
  let (pre, post) = span (not . null) xs
  in  pre : splitBlocks (drop 1 post)

addBlock :: [String] -> Int
addBlock = sum . map read


insert :: Ord a => a -> [a]  -> [a]
insert x [] = [x]
insert x (y: ys)
  | x >= y   = x : y : ys
  | x < y    = y : (insert x ys)

keep3 :: [Int] -> Int -> [Int]
keep3 xs x = take 3 $ insert x xs

solveProblem :: IO Int
solveProblem = readFile "input1.txt" <&> (sum . foldl keep3 [] . map addBlock . splitBlocks . lines)



