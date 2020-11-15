module Exercise6 where

import qualified Data.Map as M
import Data.Tuple(swap)
import Util

type ObjectName = String

-- The Orbit gives, for each object, which other object it orbits around.
type Orbit = M.Map ObjectName ObjectName

parseOrbit :: [(String, String)] -> Orbit
parseOrbit = M.fromList . map swap

{--
- Problem 1: What is the total number of direct and indirect orbits in your map data?
-
-}

{- Our algorithm: we are going to label each planet with its distance to the COM root object.
- The sum of direct/indirect orbits is then the sum of the distances of all objects.
-
- Levels: for each object in space , what is its distance to the COM.
-}

type Levels = M.Map ObjectName Int
type Boundary  = (Levels, Orbit)

{- Labelling process: at each step, we look at unlabelled objects (the orbit)
- We separate mature ones, whose "father" is already in the tally, from the rest.
- those we label with the tallt of thr 
-}
processBoundary :: Boundary -> Maybe Boundary
processBoundary (tally, orbit) =
  if M.null orbit then Nothing else Just (tally `M.union` expansion, immature)
  where
    (mature, immature) = M.partition (`M.member` tally) orbit
    -- M.partition works on the _values_ of a map
    levelOf obj = 1 + maybe 0 id (obj `M.lookup` tally)
    expansion   = M.map levelOf mature

processOrbit :: Orbit -> Levels
processOrbit = fst . last . unfoldMb processBoundary . initBoundary where 
  initBoundary orbit = (M.singleton "COM" 0, orbit)

{- What is the total number of direct and indirect orbits -}
problem1 = sum . M.elems . processOrbit . parseOrbit

testInput :: [(String, String)]
testInput = [ ("COM" , "B"), ("B" , "C"), ("C" , "D"), ("D" , "E"), ("E" , "F"), ("B" , "G"), ("G" , "H"), ("D" , "I"), ("E" , "J"), ("J" , "K"), ("K" , "L")]

testInput2 = testInput ++ [("K", "YOU"), ("I","SAN")]

-- problem2 finding the path from you to Santa. it is matter of finding closest ancestor.
chain :: Orbit -> String -> [String]
chain orb = reverse . tail . unfoldMb (`M.lookup` orb)

distance :: Orbit -> ObjectName -> ObjectName -> Int
distance orb from to =
  let chainFrom = chain orb from
      chainTo   = chain orb to
      common    = length . takeWhile (id) $ zipWith (==) chainFrom chainTo
  in length (drop common chainFrom)  + length (drop common chainTo)
