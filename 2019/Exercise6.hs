module Exercise6 where

import qualified Data.Map as M
import qualified Data.Maybe as Mb

type Orbit = M.Map String String

parseOrbit :: [(String, String)] -> Orbit
parseOrbit = M.fromList . map Data.Tuple.swap

unfoldMb :: (a -> Maybe a) -> a -> [a]
unfoldMb f = Mb.catMaybes . takeWhile Mb.isJust . iterate (>>= f) . Just

type Tally = M.Map String Int

-- problem1 find sum of direct/indirect orbits: we can do incrementally by each graph

processBoundary :: (Orbit, Tally) -> Maybe (Orbit, Tally)
processBoundary (orbit, tally) = if M.null orbit then Nothing else Just (immature, newTally)
  where 
    (mature, immature) = M.partition (`M.member` tally) orbit
    ascendents :: String -> Int
    ascendents = (1 +) . maybe 0 id . (`M.lookup` tally) 
    newTally = tally `M.union` M.map ascendents mature

processOrbit :: Orbit -> Tally
processOrbit orbit = snd . last . unfoldMb processBoundary $ (orbit, M.singleton "COM" 0)

problem1 = sum . M.elems . processOrbit . parseOrbit

testInput :: [(String, String)]
testInput = [ ("COM" , "B"), ("B" , "C"), ("C" , "D"), ("D" , "E"), ("E" , "F"), ("B" , "G"), ("G" , "H"), ("D" , "I"), ("E" , "J"), ("J" , "K"), ("K" , "L")]

testInput2 = testInput ++ [("K", "YOU"), ("I","SAN")]

-- problem2 finding the path from you to Santa. it is matter of finding closest ancestor.

chain :: Orbit -> String -> [String]
chain orb = reverse . tail . unfoldMb (`M.lookup` orb)

distance :: Orbit -> String -> String -> Int
distance orb from to =
  let chainFrom = chain orb from
      chainTo   = chain orb to
      common    = length . takeWhile (uncurry (==)) $ zip chainFrom chainTo
  in length (drop common chainFrom)  + length (drop common chainTo)
