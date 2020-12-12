module Exercise11 where

import Data.Functor((<&>))
import Data.List(inits, tails, transpose)
import Util

-- A seat is either Empty, occupied, or the floor.
data Seat = E | O | F  deriving (Eq, Show)

type Board a = [[a]]

parseSeat :: Char -> Seat
parseSeat 'L'  = E
parseSeat '#'  = O
parseSeat '.'  = F

writeSeat :: Seat -> Char
writeSeat E = 'L'
writeSeat O = '#'
writeSeat F = '.'

{- king: the movements of the chess king.
-}
type King a b =
  ( (a, a, a)
  , (a, b, a)
  , (a, a, a)
  )

-- Zipper: from a list xs = us ++ [x] ++ vs
type Zipper a = ([a], a, [a])

mapZipper :: (a -> b) -> Zipper a -> Zipper b
mapZipper f (xs, y, zs) = (map f xs, f y, map f zs)

border :: King a b -> [a]
border ( (nw, n, ne), (w, z, e), (sw, s, se)) =  [nw, n, ne, w, e, sw, s, se]

center :: King a b -> b
center  ( _, (_, z, _), _ ) = z

{-
    If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.
-}
evolve :: King Seat Seat -> Seat
evolve env = evolveAux where
  occupied = count O (border env)
  z = center env
  evolveAux
    | z == E && occupied == 0 = O
    | z == O && occupied >= 4 = E
    | otherwise               = z

parseInput :: IO (Board Seat)
parseInput = readFile "input11.txt" <&> ( map (map parseSeat) . filter (not . null) . lines)

-- nexts empties: get lists of tuples (pre-row, row, post-row) with fodders
-- uncurry zip3: 
adjacency :: a -> [[a]] -> [[King a a]]
adjacency e mat = 
  let empties = map (const e) (head mat)
  in  map (nexts (e, e, e)  . uncurry3 zip3) . nexts empties $ mat

-- given "fodder" element, zip each element with predecessor and successor
nexts :: a -> [a] -> [(a, a, a)]
nexts e xs = zip3 (precs xs) xs (succs xs) where
  precs = (e:) . init
  succs = (++ [e]) . tail

{-
Simulate your seating area by applying the seating rules repeatedly
until no seats change state. How many seats end up occupied?
-}
problem1 :: Board Seat -> Int
problem1 = occupied . fixpoint (map (map evolve) . adjacency F)

occupied :: Board Seat -> Int
occupied = sum . map (count O)

testInput :: [[Seat]]
testInput  = map (map parseSeat)
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"
  ]

{- PART 2: The long-view.
- 
-- Part 2 is similar to the Part 1. All we need to do is think of the
-- adjacent queen views (as the Chess queen moves) rather than the King view.
--
-- So we now need "queen visibility": the rows to east and west,
-- the columns to the north and south, and the diagonals to the
-- north-east, north-west, south-east and south-west. 
--
-- Also, in each line of vision we don't care about whole line, only "is there occupied"?
-- so we can in fact fold them...
--
-- so how do we build this? well, we just need to zip each elements not
-- with one preceeding and one succeeding, but with all... inits and tails.
-}

--
-- Quadrant: for any point in a grid, you have rays going
-- north, south, east, and west. Which split the grid into
-- four regions, to the North West, North East, South West,
-- and south east.
--
--                     North
--             North     |      North
--             West      |      East
--        West ------- Center------ East
--             South     |     South
--             West      |     East
--                     South
--
type Quadrant a =
  ( ([[a]], [a], [[a]])
  , ( [a],   a,   [a])
  , ([[a]], [a], [[a]])
  )

-- Queen: this is the view from a Queen: line in each direction
type Queen a =
  ( ([a], [a], [a])
  , ([a],  a,  [a])
  , ([a], [a], [a])
  )

prefices :: a -> [a] -> [[a]]
prefices e = map (e:) . init . inits

suffices :: a -> [a] -> [[a]]
suffices e = map (++ [e]) . tail . tails

zippers :: a -> [a] -> [Zipper a]
zippers e xs = zip3 (prefices e xs) xs (suffices e xs)

-- Diagonal: given matrix as, get elements (as !! 0 !! 0), (as !! 1 !! 1), etc...
diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal ([]:_) = []
diagonal ( (x:_): ys) = x : diagonal (drop 1 `map` ys)

rowZippers :: a -> [[a]] -> [Zipper [a]]
rowZippers _ [] = []
rowZippers e mat @(xs:_) = zippers empties mat
  where empties = map (const e) xs

quadrants :: a -> Zipper [a] -> [Quadrant a]
quadrants e (norths, row, souths) =
  let nnx = rowZippers e $ transpose norths
      ssx = rowZippers e $ transpose souths
  in  zip3 nnx (zippers e row) ssx

-- A queen
--
queen :: Quadrant a -> Queen a
queen ( (nwm, n, nem)
      , (w, c, e)
      , (swm, s, sem)
      ) =
  let nw = diagonal . reverse . map reverse $ nwm
      ne = diagonal . map reverse $ nem
      sw = diagonal . reverse $ swm
      se = diagonal $ sem
  in  ((nw, reverse n, ne)
      , (reverse w, c, e)
      , (sw, s, se)
      )

{-
> it now takes five or more visible occupied seats for an occupied seat
> to become empty [...]. The other rules still apply: empty seats that
> see no occupied  seats become occupied, seats matching no rule don't change,
> and floor never changes.
-}
evolveB :: Queen Seat -> Seat
evolveB env =
  let occupied = countBy isFirstOccupied (border env)
      isFirstOccupied = elem O . take 1 . filter (/= F)
      z = center env
      evolveAux
        | center env == E && occupied == 0   = O
        | center env == O && occupied >= 5   = E
        | otherwise                          = z
  in evolveAux

queenView :: a -> [[a]] -> [[Queen a]]
queenView e = map (map queen . quadrants e) . rowZippers e

{-
- Simulate your seating area by applying the seating rules repeatedly
- until no seats change state. How many seats end up occupied?
-}
problem2 :: Board Seat -> Int
problem2 = occupied . fixpoint (map (map evolveB) . queenView F)
