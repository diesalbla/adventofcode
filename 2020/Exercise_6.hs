module Exercise6 where

import Util(splitAtElem)
import qualified Data.Set as S
import Data.Functor((<&>))

{--
Another group asks for your help, then another, and eventually you've
collected answers from every group on the plane (your puzzle
input).
Each group's answers are separated by a blank line, and within
each group, each person's answers are on a single line. For example:
-}
problem :: Ord a => (S.Set a -> S.Set a -> S.Set a) -> [[[a]]] -> Int
problem mer = sum . map (S.size . foldl1 mer . map S.fromList)

partA :: [[String]] -> Int
partA = problem S.union
partB :: [[String]] -> Int
partB = problem S.intersection

runProblem :: (S.Set Char -> S.Set Char -> S.Set Char) -> IO ()
runProblem mer =
  readFile "input6.txt"
  <&>  (show . problem mer . splitAtElem "" . lines)
  >>= putStrLn
