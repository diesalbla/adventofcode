module Util where

import qualified Data.Maybe as Mb

unfoldMb :: (a -> Maybe a) -> a -> [a]
unfoldMb f = Mb.catMaybes . takeWhile Mb.isJust . iterate (>>= f) . Just

