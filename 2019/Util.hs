module Util where

import qualified Data.Maybe as Mb

unfoldMb :: (a -> Maybe a) -> a -> [a]
unfoldMb f = Mb.catMaybes . takeWhile Mb.isJust . iterate (>>= f) . Just

-- replace ix x xs:  item at postition ix by t
setAt :: Int -> a -> [a] -> [a]
setAt _ _ [] = []
setAt 0 y (_:xs) = y:xs
setAt i y (x:xs) = x : setAt (i-1) y xs

