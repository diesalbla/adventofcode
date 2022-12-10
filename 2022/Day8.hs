

module Day8 where

import Data.List(inits, tails, transpose)
import Data.Functor((<&>))
import Data.Char(ord)

-- visible trees : a tree is visible if
-- other trees in its vicinity

-- Solution: tuple each row with predecessors, successors:: [ ([
--

data Cross a = Cross { elem :: a, north :: [a], south :: [a], east :: [a], west :: [a] } deriving Show

snd3 (_, y, _) = y

isCovered :: Ord a => Cross a -> Bool
isCovered (Cross x ns ss ws es) = and . map (any (>= x)) $ [ns, ss, ws, es]


crosses :: [[a]] -> [[Cross a]]
crosses = map (map toCross . rays1 . toRay2) . rays1  where 
  rays1 :: [a] -> [ ([a], a, [a]) ]
  rays1 [] = []
  rays1 xs = zip3 (init . inits $ xs) xs (tail . tails $ xs)

  toRay2 (prevs,  row,  succs) =
    let transposeAux xs = if null xs then map (\ _ -> []) row  else transpose xs
    in  zip3 (transposeAux prevs) row (transposeAux succs)
  toCross ( easts, (north, elem, south), wests) =
    Cross elem (reverse north) south (reverse . map snd3 $ easts) (map snd3 wests)

problem1 :: Ord a => [[a]] -> Int
problem1 = length . filter (not . isCovered) . concat . crosses
 
digitToInt :: Char -> Int
digitToInt x = ord x - 48

parseRow :: String -> [Int]
parseRow = map digitToInt

solveProblem = readFile "input8.txt" <&> problem1 . map parseRow . lines

show3 :: (Show a, Show b, Show c) => (a, b, c) -> [String]
show3 (x, y, z) = ["(" , "  " ++ show x, "  " ++ show y, "  " ++ show z,  ")"]

example :: [[Int]]
example =
  [
    [3, 0, 3, 7, 3],
    [2, 5, 5, 1, 2],
    [6, 5, 3, 3, 2],
    [3, 3, 5, 4, 9],
    [3, 5, 3, 9, 0]
  ]


-- For problem2 we have one problem: find number of elements until

takeThrough :: (a -> Bool) -> [a] -> [a]
takeThrough p xs = let (ys, zs) = span p xs in ys ++ take 1 zs

{--
> A tree's scenic score is found by multiplying together its viewing distance in
> each of the four directions.
--}
scenicScore :: Ord a => Cross a -> Int
scenicScore (Cross x ns ss ws es) =
  product . map (length . takeThrough (< x)) $ [ns, ss, ws, es]

{--
> Consider each tree on your map. What is the highest scenic score possible for any tree?
--}

problem2 :: Ord a => [[a]] -> Int
problem2 = maximum . map scenicScore .  concat . crosses
