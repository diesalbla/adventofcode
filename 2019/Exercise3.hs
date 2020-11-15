module Exercise3 where

type Point = (Int, Int)

point x y = (x, y)

-- manhattan distance between any two points.
manhattan :: Point -> Point -> Int
manhattan (xa, ya) (xb, yb) = abs (xa - xb) + abs (ya - yb)

-- distance between two points on same row or column
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
west  = east  . negate

{-- Move from a Point by a given Vector
--}
add :: Vector -> Point -> Point
add (d, n) (x, y) = case d of
  E -> (x + n, y)
  N -> (x, y + n)

type Segment = (Point, Vector)


{- An Interval is an opn pair of numbers (a, b), with a <= b,
-- representing closed set 
-}
data Interval = Interval { low :: Int, upp : Int }

-- make interval from point and movement 
interval :: Int -> Int -> Interval
interval x n = if n > 0 then Interval x  (x + n) else Interval (x + n) x

-- Does the close interval (x, y) contain value `c`? 
contains :: Interval -> Int -> Bool
contains (Interval l u) c = (l <= c) && (c <= u)

points :: Interval -> [Int]
points (Interval l u) = [l .. u]

intersect :: Interval -> Interval -> Maybe Interval
intersect (Interval lowA uppA) (Interval lowB uppB) =
  let nlow = max lowA lowB
      nupp = max uppA uppB
  in  if nlow <= nupp
      then Just (Interval nlow nupp)
      else Nothing

{-
Given two segments, get the list of points at which they cross? 
-}
crossings :: Segment -> Segment -> [Point]
crossings segA segB = 
  case (da, db) of
    -- parallel 
    (N, N) -> if xa /= xb then [] else 
      map (point xa)   $ interval ya la `common` interval yb lb
    (E, E) -> if ya /= yb then [] else
      map (`point` ya) $ interval xa la `common` interval xb lb
    -- perpendicular ones may cross at one point
    (N, E) -> crossings segB segA
    (E, N) ->
      if (interval xa la) `contains` xb && (interval yb lb) `contains` ya
      then [(xb, ya)] else []
  where
    common intA intB = maybe [] points $ intersect intA intB
    ((xa, ya), (da, la)) = segA
    ((xb, yb), (db, lb)) = segB

problem1 :: [Vector] -> [Vector] -> Int
problem1 moves1 moves2 =
  let move (pos, tr) vec = (add vec pos, (pos, vec) : tr) 
      trace = snd . foldl move ( (0,0), [])
      cross = crossings <$> trace moves1 <*> trace moves2
  in  minimum . filter (>0) . map (manhattan (0,0)) . concat $ cross

{--
-- problem 2: we need to keep track of the "distance" to each intersection...
-- so now each segment needs to keep the added cost
-}

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
