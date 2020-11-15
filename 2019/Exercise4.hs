{-# LANGUAGE DuplicateRecordFields, DisambiguateRecordFields #-}
module Exercise4 where

import Data.Function(on)
import Data.List(group)

{--
[...] key facts about the password:

 - It is a six-digit number.
 - The value is within the range given in your puzzle input.
 - Two adjacent digits are the same (like 22 in 122345).
 - Going from left to right, the digits never decrease;
   they only ever increase or stay the same (like 111123 or 135679).
--}
digits :: Int -> [Int]
digits x = if x == 0 then [] else x `mod` 10 : digits (x `quot` 10 )

-- growing: given list of non-empty lists, they are growuing
growing :: Ord a => [[a]] -> Bool
growing gs = and $ zipWith ( (>) `on` head) gs (tail gs)

{-
-- 
-}
isGoodA :: Int -> Bool
isGoodA num = onePair && growing gs where
  gs      = group . digits $ num
  onePair = any ( (>= 2) . length) gs

part1 = length . filter isGoodA $ [172851..675869]

{-
-- [...] A one more important detail: the two adjacent matching digits
-- are not part of a larger group of matching digits.
-}
isGoodB :: Int -> Bool
isGoodB num = onePair && growing gs where
  gs      = group . digits $ num
  onePair = any ( (== 2) . length) gs

part2 = length . filter isGoodB $ [172851..675869]
