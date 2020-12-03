module Exercise3 where

-- The map: trees or not, Boolean
-- The map repeats itself: just module.
-- 

-- Our forest is a grid of booleans, where True is there is tree, False means open
-- 
type Forest = [[Bool]]

parseCell :: Char -> Bool
parseCell '.' = False
parseCell '#' = True

parseForest :: [String] -> Forest
parseForest = map (map parseCell)

-- collides n  position n 
collides :: Int -> [Bool] -> Bool
collides ix bs = bs !! (ix `mod` length bs)

divides :: Int -> Int -> Bool
divides n m = m `mod` n == 0

-- checkSlope hor ver forest
-- Given the slope of going `hor` positions right, `ver` positions down,
-- what do we get in our forest? 
checkSlope :: Int -> Int -> Forest -> Int
checkSlope hor ver =
  length
  . filter id
  . zipWith collides (iterate (hor+) 0)
  . map snd
  . filter (divides ver . fst)
  . zip [0..] 

problem1 = checkSlope 3 1

problem2 forest = product . map check $ [(1,1), (3, 1), (5, 1), (7, 1) , (1, 2)] where
  check (h, v) = checkSlope h v forest

runProblem =
  do text <- readFile "input3.txt"
     let forest = parseForest . filter (not . null) . lines $ text
     let answer = problem2 forest
     putStrLn (show answer)
