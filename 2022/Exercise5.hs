module Exercise5 where

import qualified Data.Map as M
import Data.Functor((<&>))
import Data.Maybe(catMaybes)
import Data.List(transpose)
import Data.Char(isDigit)

-- In the Dock, each Stack is with top-level at head
type Dock = [[Char] ]

data Move = Move { num :: Int, from :: Int, to :: Int } deriving (Eq, Show)

moveMany :: Dock -> Move -> Dock
moveMany st (Move num from to) =
  let crates = take num $ st !! (from - 1)
  in  replaceAtBy from (drop num) 
      . replaceAtBy to (\hs ->  (crates ++ hs)) 
      $ st

ntimes :: Int -> (a -> a) -> (a -> a)
ntimes n f
  | n <= 0  = id
  | n == 1  = f
  | n > 1   = f . ntimes (n-1) f 

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n x xs =
  let (pre, post) = splitAt (n - 1) xs
  in pre ++ [x] ++ tail post 

replaceAtBy :: Int -> (a -> a) -> [a] -> [a]
replaceAtBy n f xs =
  let (pre, (x: post)) = splitAt (n - 1) xs
  in pre ++ [f x] ++ post 

{-- How to parse the state? we have stacks from top to bottom...
so, we need to make each line into a List of Maybe Char.
then we need to compile the list of lists of maybes into the stacks

--}

compileDock :: [[Maybe Char]] -> Dock
compileDock  = map catMaybes . transpose

splitDockLine :: String -> [String]
splitDockLine [] = []
splitDockLine xs =
  let (pre, post) = splitAt 3 xs
      rest = if null post then [] else splitDockLine (tail post)
  in  pre : rest

 -- Each entry are three characers
parseDockEntry :: String -> Maybe Char
parseDockEntry ['[', x, ']']  = Just x
parseDockEntry "   "          = Nothing

parseDockLine :: String -> [Maybe Char]
parseDockLine = map parseDockEntry . splitDockLine

-- How to parse: "move NUM from FROM to TO"
parseMove :: String -> Move
parseMove str =
  let ("move ", s1)  = splitAt 5 str
      (num, s2)      = span isDigit s1
      (" from ", s3) = splitAt 6 s2
      (from, s4)     = span isDigit s3
      (" to ", to)   = splitAt 4 s4
  in  Move (read num) (read from) (read to)
  
isNumbersLine :: String -> Bool
isNumbersLine = all (\c -> isDigit c || c == ' ')

isDockLine :: String -> Bool
isDockLine = any (\c -> c == '[' || c == ']')

problem1 input =
  let (dockLines, input1) = span isDockLine (lines input)
      moveLines = dropWhile (not . isMove) input1
      isMove = (== "move ") . take 5
      initDock  = compileDock . map parseDockLine $ dockLines
  in  map head . foldl moveMany initDock . map parseMove $ moveLines

solveProblem = readFile "input5.txt" <&> problem1 -- >>= mapM_ putStrLn
