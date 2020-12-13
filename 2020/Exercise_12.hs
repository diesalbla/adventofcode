module Exercise12 where

import Data.Functor((<&>))

{- Solving Exercise 12 is a matter of using vectors in 2 dimensions,
-  in this case only integer vectors. Both positions and waypoints
- (velocity) are represented with vectors. 
-
-}

data Vec = Vec { x :: Int, y :: Int } deriving (Eq)

vadd :: Vec -> Vec -> Vec
vadd (Vec a b) (Vec c d) = Vec (a+c) (b+d)

-- Rotate a vector with number of degrees
rotate :: Int -> Vec -> Vec
rotate degs (Vec x y) = 
  case (degs `mod` 360) `quot` 90 of
    0 -> Vec  x  y
    1 -> Vec (-y)  x
    2 -> Vec (-x) (-y)
    3 -> Vec  y (-x)

scale :: Int -> Vec -> Vec
scale k (Vec x y) = Vec (k*x) (k*y)

manhattan :: Vec -> Int
manhattan (Vec x y) = abs x + abs y

--
-- A Ship state is represented with two vectors: 
-- its position, and its direction (speed)
--
data Ship = Ship { position :: Vec, steer :: Vec }

startShip = Ship (Vec 0 0) (Vec 1 0)

data Move = North Int
          | South Int
          | West  Int
          | East  Int
          | LeftT  Int
          | RightT Int
          | Forward Int

parseMove :: String -> Move
parseMove (alpha:digs) = constr (read digs) where
  constr = case alpha of
    'N' -> North
    'S' -> South
    'W' -> West
    'E' -> East
    'R' -> RightT
    'L' -> LeftT
    'F' -> Forward

-- The operations north south east and west represent
-- vectors to be added. 
toVector :: Move -> Vec
toVector (North n) = Vec 0 n
toVector (South n) = Vec 0 (-n)
toVector (West n)  = Vec (-n) 0
toVector (East n)  = Vec n 0
toVector _ = Vec 0 0 

doMoveA :: Ship -> Move -> Ship
doMoveA (Ship pos course) (Forward n) =
  Ship (pos `vadd` (n `scale` course)) course
doMoveA (Ship pos course) move = 
  case move of
    RightT n -> Ship pos (rotate (-n) course)
    LeftT  n -> Ship pos (rotate  n course)
    _ -> Ship (pos `vadd` toVector move) course

testInput = [ "F10", "N3", "F7", "R90", "F11"]

problem1 :: [Move] -> Int
problem1 = manhattan . position . foldl doMoveA startShip

--
-- Problem2 :: the ship state is the WayPoint, so in a way needs redoing
--
changeCourse :: Move -> Vec -> Vec
changeCourse (Forward n) wp  = wp
changeCourse (RightT  n) wp = rotate (-n) wp
changeCourse (LeftT   n) wp = rotate n wp
changeCourse move wp = wp `vadd` toVector move

-- The waypoint starts 10 units east and 1 unit north
startShipB = Ship (0 `Vec` 0) (10 `Vec` 1)

doMoveB :: Ship -> Move -> Ship
doMoveB (Ship pos course) (Forward n) =
  Ship (pos `vadd` (n `scale` course)) course
doMoveB (Ship pos course) move =
  Ship pos (changeCourse move course)

problem2 = manhattan . position . foldl doMoveB startShipB

parseInput :: IO [Move]
parseInput = readFile "input12.txt" <&> ( map parseMove . filter (not . null) . lines)

runProblem1 = parseInput >>= (putStrLn . show . problem1 )
runProblem2 = parseInput >>= (putStrLn . show . problem2 )
