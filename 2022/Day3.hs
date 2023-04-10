module Exercise3 where

import Data.Functor((<&>))
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Char(isLower, isUpper, ord)
import Data.Maybe(catMaybes)

{--

Problem 1: compartimentalisation.

Given two string of characters, let the intersection be the common items.
The idea is: given a string s, find the index i such that s[0..i) and s[i..N)
have a minimal intersection of one element


Simple combinatorics solution: try each position, get solution, get count

More stylised proposition: start with the map (all on one place) and
run through the string on a subtract / detract

--}

type Inventory = M.Map Char Int

addChar :: Char -> Inventory -> Inventory
addChar c = M.insertWith (+) c 1 

inventory :: String -> Inventory
inventory = foldr addChar M.empty 

commonItem :: String -> Char
commonItem str = 
  let len = length str
      (pre, post) = L.splitAt (len `div` 2) str
   in head $ L.intersect pre post

{--
To help prioritize item rearrangement, every item type can be converted to a priority:

    Lowercase item types a through z have priorities 1 through 26.
    Uppercase item types A through Z have priorities 27 through 52.

--}
priority :: Char -> Maybe Int
priority x
  | isLower x = Just $ ord x - 96
  | isUpper x = Just $ ord x - 65 + 27
  | otherwise = Nothing

{--
> Find the item type that appears in both compartments of each rucksack.
> What is the sum of the priorities of those item types?
--}

problem1 = sum . catMaybes . map (priority . commonItem) . lines

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

problem2 :: String -> Int
problem2 = sum . catMaybes . map (priority . head . foldl1 L.intersect ) . groupsOf 3 . lines

solveProblem = readFile "input3.txt" <&> problem2
