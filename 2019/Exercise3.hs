
module Exercise3 where

type Point = (Int, Int)

point x y = (x, y)

manhattan :: Point -> Point -> Int
manhattan (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

distance :: Point -> Point -> Int
distance (xa, ya) (xb, yb)
  | xa == xb   = abs (yb - ya)
  | ya == yb   = abs (xb - xa)
  | otherwise  = error "Distance of non-aligned points"

data Dir = N | E deriving (Eq, Ord, Show)
type Vector = (Dir, Int)

north = (,) N
east  = (,) E
south = north . negate
west = east . negate

add :: Vector -> Point -> Point
add (d, n) (x, y) = case d of
  E -> (x + n, y)
  N -> (x, y + n)

type Segment = (Point, Vector)

-- Do these segments cross at any point? 
crossings :: Segment -> Segment -> [Point]
crossings segA segB = 
  case (da, db) of
    (N, N) -> if xa /= xb then [] else 
      map (point xa)   $ interval ya la `intersect` interval yb lb
    (E, E) -> if ya /= yb then [] else
      map (`point` ya) $ interval xa la `intersect` interval xb lb
    (N, E) -> crossings segB segA
    (E, N) ->
      if (interval xa la) `contains` xb && (interval yb lb) `contains` ya
      then [(xb, ya)] else []
  where
    intersect (lowA, highA) (lowB, highB) = [lowA `max` lowB .. highA `min` highB]
    interval x n = if n > 0 then (x, x + n) else (x + n, x)
    contains (a, b) c = (a <= c) && (c <= b)
    ((xa, ya), (da, la)) = segA
    ((xb, yb), (db, lb)) = segB

problem1 :: [Vector] -> [Vector] -> Int
problem1 moves1 moves2 =
  let move (pos, tr) vec = (add vec pos, (pos, vec) : tr) 
      trace = snd . foldl move ( (0,0), [])
      cross = crossings <$> trace moves1 <*> trace moves2
  in  minimum . filter (>0) . map (manhattan (0,0)) . concat $ cross

-- problem 2: we need to keep track of the "distance" to each intersection...
-- so now each segment needs to keep the added cost

move2 :: (Point, [(Segment, Int)]) -> Vector -> (Point, [(Segment, Int)])
move2 (pos, tra) vec = (add vec pos, nframe:tra) where 
  nframe = ((pos, vec), acc)
  acc  = case tra of
    [] -> 0
    ( (_, (_, n)), acc):_ -> acc + abs n
      

--problem2 :: [Vector] -> [Vector] -> Int
problem2 moves1 moves2 =
  let trace = snd . foldl move2 ( (0,0), [])
      costCross :: (Segment, Int) -> (Segment, Int) -> Point -> Int
      costCross (seg1, acc1) (seg2, acc2) cr =
        acc1 + acc2 + distance (fst seg1) cr + distance (fst seg2) cr
      costCrossings p1@(seg1, acc1) p2@(seg2, acc2) =
        map (costCross p1 p2)  (crossings seg1 seg2)
      cross = costCrossings <$> trace moves1 <*> trace moves2
  in  minimum . filter (> 0) . concat $ cross

testBase1 :: [Vector]
testBase1 = [east 8, north 5, west 5, south 3]

testBase2 :: [Vector]
testBase2 = [north 7,east 6,south 4,west 4]

testA1 :: [Vector]
testA1 = [east 75,south 30,east 83,north 83,west 12,south 49,east 71,north 7,west 72]
testA2 :: [Vector]
testA2 = [north 62,east 66,north 55,east 34,south 71,east 55,south 58,east 83]

input1 :: [Vector]
input1 = undefined -- TODO your input here 
input2 :: [Vector]
input2 = undefined -- TODO your input here
