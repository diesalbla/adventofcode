
module Day1 where

import Data.Char(isDigit, digitToInt, intToDigit)
import Data.List(splitAt)
import Data.Maybe(catMaybes, isJust, listToMaybe)
import Data.Functor((<&>))

calibrationValue :: String -> Int
calibrationValue str = 10 * head ds + last ds where
  ds = map digitToInt . filter isDigit $ str

part1 = readFile "input1.txt" <&>  sum . map calibrationValue . lines 

-- For Part 2, we need to add the replace function: words to numbers

replacePrefix :: (String, Int) -> String -> Maybe Int
replacePrefix (pat, d)  str =
  let (pre,suf) = splitAt (length pat) str
  in if pre == pat then Just d else Nothing

replaceDigits :: String -> String
replaceDigits "" = ""
replaceDigits str = head str : ( replaceOne str ++ replaceDigits (tail str) )where
  replaceOne = map intToDigit . take 1 . catMaybes . attemptAll 
  attemptAll s = map (`replacePrefix` s) digitWords

oneTens = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitWords = oneTens `zip` [1..9]

part2 = readFile "input1.txt" <&>  sum . map (calibrationValue . replaceDigits) . lines 
