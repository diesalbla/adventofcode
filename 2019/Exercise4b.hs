module Exercise4B where

import Data.Foldable(foldlM)

-- problem part 2: there is group of two that is not group of three
-- So we need state machine: (two previous + seen 2 without 3).

digits :: Int -> [Int]
digits x = if x == 0 then [] else x `mod` 10 : digits (x `quot` 10 )

data State = State { lastNum :: Int , repeated :: Int , sawTwoGroup :: Bool }

step2 :: State -> Int -> Maybe State
step2 (State x n b) y = case compare x y of
  LT -> Nothing
  EQ -> Just $ State x (n+1) b
  GT -> Just $ State y 1 (b || n == 2)

isGood2 :: Int -> Bool
isGood2 = maybe False isFinal . foldlM step2 initState . digits where
  initState = (State 9 0 False)
  isFinal st = sawTwoGroup st || repeated st == 2


