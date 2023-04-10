module Exercise4 where

import Data.Functor((<&>))
import Data.Char(isDigit)

type Range = (Int, Int)

-- [c --- [a --- b] --- d]
isSub :: Range -> Range -> Bool
isSub (a, b) (c, d) = c <= a && b <= d

-- [a --- c --- b]
inside :: Int -> Range -> Bool
inside c (a, b) = a <= c && c <= b

consider :: Range -> Range -> Bool
consider r s = r `isSub` s  || s `isSub` r

parse :: String -> (Range, Range)
parse str =
  let (a, s1) = span isDigit str
      (_, s2) = span (== '-') s1
      (b, s3) = span isDigit  s2
      (_, s4) = span (== ',') s3
      (c, s5) = span isDigit  s4
      (_, s6) = span (== '-') s5
      (d, s7) = span isDigit  s6
  in ((read a, read b), (read c, read d))
                     

problem1 :: String -> Int
problem1 = length . filter (uncurry consider) . map parse . lines

--  [a -- (c -- b ] -- d)
  --  (c -- [a -- d) -- b]
-- 
overlap :: Range -> Range -> Bool
overlap (a, b) (c, d) =
  c `inside` (a, b) ||
  d `inside` (a, b) ||
  a `inside` (c, d) ||
  b `inside` (c, d)
  

problem2 :: String -> Int
problem2 = length . filter (uncurry overlap) . map parse . lines

solveProblem = readFile "input4.txt" <&> problem2
