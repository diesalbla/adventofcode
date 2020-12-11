module Util where

import qualified Data.Maybe as Mb

splitAtElem :: Eq a => a -> [a] -> [[a]]
splitAtElem _ [] = []
splitAtElem e as =
  let (pre, suff) = span (/= e) as
      pref = if null pre then [] else [pre]
      sufx = splitAtElem e (dropWhile (== e) suff)
  in  pref ++ sufx

unfoldMb :: (a -> Maybe a) -> a -> [a]
unfoldMb f = Mb.catMaybes . takeWhile Mb.isJust . iterate (>>= f) . Just

longerThan :: Int -> [a] -> Bool
longerThan len = (== len) . length . take len

countBy :: (a -> Bool) -> [a] -> Int
countBy p = length . filter p

count :: Eq a => a -> [a] -> Int
count a = countBy (== a)
