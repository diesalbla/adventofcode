module Util where


splitAtElem :: Eq a => a -> [a] -> [[a]]
splitAtElem _ [] = []
splitAtElem e as =
  let (pre, suff) = span (/= e) as
      pref = if null pre then [] else [pre]
      sufx = splitAtElem e (dropWhile (== e) suff)
  in  pref ++ sufx

