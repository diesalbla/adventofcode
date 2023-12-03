
module Day3 where

import Data.Char(isDigit)
import Data.Functor((<&>))
import Data.List(zip3, break, elem)

-- We mark horizontal segments within the matrix using (row, column, length)
type Segment = (Int, Int, Int) -- row, column, length

readSegment :: Segment -> [String] -> Int
readSegment (row, col, len) =  read . take len . drop col . (!! row)

regionsWhere :: (a -> Bool) -> [a] -> [(Int, Int)]
regionsWhere p = go 0 where
  go _ [] = []
  go n (x: xs)
    | p x =
        let (pre, post) = span p xs
            len = 1 + length pre
        in  (n, len) : go (n + len) post
    | otherwise = go (n+1) xs

regionsWhereMatrix :: (a -> Bool) -> [[a]] -> [(Int, Int, Int)]
regionsWhereMatrix pred = concat . zipWith regionsWithRow (enumFrom 0) where
  regionsWithRow row = map (tupleCons row) . regionsWhere pred
  tupleCons x (y, z) = (x, y, z)

part1 :: [String] -> Int
part1 ss = sum . map (`readSegment` ss) . filter isPart $ piecs where
  syms =  regionsWhereMatrix isSymbol ss
  piecs = regionsWhereMatrix isDigit ss
  isPart piece = any (adjacentRegions piece) syms
  isSymbol c = not  (isDigit c || c == '.')

adjacentRegions :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
adjacentRegions (ra, ca, la) (rb, cb, lb) =
  adjacent ra rb && (adjacent (ca + la - 1) cb || adjacent ca (cb + lb - 1) )

adjacent :: Int -> Int -> Bool
adjacent x y = abs (x - y) <= 1

findGears :: [String] -> [Int]
findGears ss = concatMap maybeGear stars where
  stars = regionsWhereMatrix (== '*') ss
  piecs = regionsWhereMatrix isDigit ss
  maybeGear star =
    case filter (adjacentRegions star) piecs of
      [x, y] -> [readSegment r1 ss * readSegment r2 ss]
      _ -> []
  toPoint (x, y, _) = (x, y)

part2 :: [String] -> Int
part2 ss = sum .  findGears $ ss
