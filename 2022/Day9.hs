module Day9 where

import Data.List(nub)
import Data.Functor((<&>))

{--
We have moves and State.
The moves take place one by one,

Each state is two pairs: head, tail. 

 - the current move determines head. 
 - tail follows current head is too far apart.

--}


type Position = (Int, Int)

data Rope = Rope { knots :: [Position] } deriving (Eq, Show)

initRope :: Int -> Rope
initRope = Rope . listOfN (0, 0) 

data Direction = U | D | R | L deriving (Eq, Show, Read)

move :: Position -> Direction -> Position
move (x, y)  dir = case dir of
  U -> (x, y + 1)
  D -> (x, y - 1)
  R -> (x + 1, y)
  L -> (x - 1, y)


-- Follow: tail follows 
heading :: Position -> Position -> [Direction]
heading (hx, hy) (tx, ty) = if distance < 2 then [] else vertical ++ horizontal where
  vertical 
    | ty < hy   = [U]
    | ty > hy   = [D]
    | otherwise = []
  horizontal 
    | tx < hx   = [R]
    | tx > hx   = [L]
    | otherwise = []
  distance = abs (hx - tx) `max` abs (hy - ty)

follow :: Position -> Position -> Position
follow pre post = foldl move post (heading pre post)

step :: Rope -> Direction -> Rope
step (Rope (hk:ts)) dir = Rope . scanl follow (move hk dir) $ ts
  

-- steps :: Int -> Direction -> Rope -> [Rope]
-- steps n dir rope = foldl step rope (listOfN n dir)

listOfN :: a -> Int -> [a]
listOfN a n = take n $ repeat a

{--
uAfter simulating the rope, you can count up all of the positions the tail visited at least once
[--}

traceRope :: Int -> [Direction] -> [Rope]
traceRope = scanl step . initRope

parseMove :: String -> (Direction, Int)
parseMove (dirChar : ' ' : num) = (read [dirChar], read num)

problem n = length . nub . map (last . knots) . traceRope n

solveProblem n = readFile "input9.txt" <&> problem n . concatMap (uncurry listOfN) . map parseMove . lines
