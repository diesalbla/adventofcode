module Exercise12 where

import Data.Functor((<&>))

-- we represent positions and directions with vectors
data Vec = Vec { x :: Int, y :: Int } deriving (Eq)

instance Num Vec where
  (Vec a b) + (Vec c d) = Vec (a+c) (b+d)
  abs (Vec a b) = Vec (abs a) (abs b)

-- Rotate a vector with number of degrees
rotate :: Int -> Vec -> Vec
rotate degs (Vec x y) = 
  case (degs `mod` 360) `quot` 90 of
    0 -> Vec  x  y
    1 -> Vec (-y)  x
    2 -> Vec (-x) (-y)
    3 -> Vec  y (-x)

scalar :: Int -> Vec -> Vec
scalar k (Vec x y) = Vec (k*x) (k*y)

data Ship = Ship { position :: Vec, steer :: Vec }

startShip = Ship (Vec 0 0) (Vec 1 0)

manhattan :: Ship -> Int
manhattan (Ship (Vec a b) _ ) = abs a + abs b

data Move = North Int
          | South Int
          | West  Int
          | East  Int
          | LeftT  Int
          | RightT Int
          | Forward Int

parseMove :: String -> Move
parseMove str =
  let (alpha : digs) = str
      constr = case alpha of
        'N' -> North
        'S' -> South
        'W' -> West
        'E' -> East
        'R' -> RightT
        'L' -> LeftT
        'F' -> Forward
  in constr $ read digs

toVector :: Move -> Vec
toVector (North n) = Vec 0 n
toVector (South n) = Vec 0 (-n)
toVector (West n)  = Vec (-n) 0
toVector (East n)  = Vec n 0
toVector _ = Vec 0 0 

doMoveA :: Ship -> Move -> Ship
doMoveA (Ship pos course) (Forward n) =
  Ship (pos + (n `scalar` course)) course
doMoveA (Ship pos course) move = 
  case move of
    RightT n -> Ship pos (rotate (-n) course)
    LeftT  n -> Ship pos (rotate  n course)
    _ -> Ship (pos + toVector move) course

testInput = [ "F10", "N3", "F7", "R90", "F11"]

problem1 :: [Move] -> Int
problem1 = manhattan . foldl doMoveA startShip

--
-- Problem2 :: the ship state is the WayPoint, so in a way needs redoing
--
changeCourse :: Move -> Vec -> Vec
changeCourse (Forward n) wp  = wp
changeCourse (RightT  n) wp = rotate (-n) wp
changeCourse (LeftT   n) wp = rotate n wp
changeCourse move wp = wp + toVector move

-- The waypoint starts 10 units east and 1 unit north
startShipB = Ship (0 `Vec` 0) (10 `Vec` 1)

doMoveB :: Ship -> Move -> Ship
doMoveB (Ship pos course) (Forward n) =
  Ship (pos + (n `scalar` course)) course
doMoveB (Ship pos course) move =
  Ship pos (changeCourse move course)

problem2 = manhattan .foldl doMoveB startShipB

parseInput :: IO [Move]
parseInput = readFile "input12.txt" <&> ( map parseMove . filter (not . null) . lines)

runProblem1 = parseInput >>= (putStrLn . show . problem1 )
runProblem2 = parseInput >>= (putStrLn . show . problem2 )

