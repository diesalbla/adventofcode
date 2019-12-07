module Exercise4 where

import Data.Foldable(foldlM)

digits :: Int -> [Int]
digits x = if x == 0 then [] else x `mod` 10 : digits (x `quot` 10 )

data State = State { lastNum :: Int , sawTwoGroup :: Bool }

step :: State -> Int -> Maybe State
step (State x b) y = case compare x y of
  LT -> Nothing
  EQ -> Just $ State x True
  GT -> Just $ State y b

isGood = maybe False sawTwoGroup . foldlM step initState . digits where
  initState = (State 99 False)
