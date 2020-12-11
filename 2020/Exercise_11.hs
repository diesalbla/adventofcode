module Exercise11 where

import Data.Functor((<&>))
import Data.List(inits, tails, transpose)
import Data.Maybe(fromMaybe)
import Util

-- A seat is either Empty, occupied, or the floor.
data Seat = E | O | F  deriving (Eq, Show)

type Board a = [[a]]

parseSeat :: Char -> Seat
parseSeat 'L'  = E
parseSeat '#'  = O
parseSeat '.'  = F

type Environ a b = ( (a, a, a) , (a, b, a), (a, a, a))
type Environ3 a b c = ( (a, b, a) , (b, c, b), (a, b, a))

-- Zipper: from a list xs = us ++ [x] ++ vs
type Zipper a = ([a], a, [a])

border :: Environ a b -> [a]
border ( (nw, n, ne), (w, z, e), (sw, s, se)) =  [nw, n, ne, w, e, sw, s, se]

center :: Environ a b -> b
center  ( _, (_, z, _), _ ) = z

{-
    If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.
-}
evolve :: Environ Seat Seat -> Seat
evolve env = evolveAux where
  occupied = count O (border env)
  z = center env
  evolveAux
    | z == E && occupied == 0 = O
    | z == O && occupied >= 4 = E
    | otherwise               = z

parseInput :: IO (Board Seat)
parseInput = readFile "input11.txt" <&> ( map (map parseSeat) . filter (not . null) . lines)

adjacency :: a -> [[a]] -> [[Environ a a]]
adjacency e mat = 
  let zcells :: ([a], [a], [a]) -> [(a, a, a)]
      zcells (prev, row, next) = zip3 prev row next

      zmats cols = zip3 ( (e, e, e) : init cols) cols (tail cols ++ [(e, e, e)])
      empties = map (const e) (head mat)

  in  map (zmats . zcells) . nexts empties $ mat

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


testInput =
  [ "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "L"
  , "LLLLLLLLLL.LLLLLL.L"
  , "L.LLLLL.LL"
  ]


-- Problem 2 : we need to think of rows columns and diagonals... hard...
-- It is similar in a way: for each seat, built an environment...
--
-- So we now need the "visibility lens":
--
-- Well, what we have to do is, for each seat  AS(i, j), we need
-- rows to the west and east : AS(i, 0..j-1), AS(i, j+1 ..N)
-- columns to north and south: AS(0..i-1, j), AS(i+1..N, j)
--
-- diagonals to northeast, northwest, southeast and southwesst.
-- For diagonals, we only nead one definition and then reverses and map reverses
--
-- 
-- Also, in each line of vision we don't care about whole line, only "is there occupied"?
-- so we can in fact fold them...
--
-- so how do we build this? well, we just need to zip each elements not
-- with one preceeding and one succeeding, but with all... inits and tails.

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

-- visibilityLens :: a -> [[a]] -> [[Environ [a]]]
-- visibilityLens e mat = map (fili e) (rowZippers e mat)

fili :: a -> Zipper [a] -> [Environ3 [[a]] [a] a]
fili e (norths, row, souths) =
  let nnx = rowZippers e . transpose $ norths
      ssx = rowZippers e . transpose $ souths
  in  zip3 nnx (zippers e row) ssx

kili :: Environ3 [[a]] [a] a -> Environ [a] a
kili ((nwm, n, nem),  (w, c, e),  (swm, s, sem)) =
  let nw = diagonal . reverse . map reverse $ nwm
      ne = diagonal . reverse $ nem
      sw = diagonal . map reverse $ swm
      se = diagonal $ sem
  in  ((nw, reverse n, ne), (reverse w, c, e) , (sw, s, se))

evolveB :: Environ [Seat] Seat -> Seat
evolveB env =
  let occupied = countBy isFirstOccupied (border env)
      isFirstOccupied = elem O . take 1 . filter (/= F)
      z = center env
      evolveAux
        | center env == E && occupied == 0   = O
        | center env == O && occupied >= 5   = E
        | otherwise                          = z
  in evolveAux

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint fun initial = fst . head . dropWhile (uncurry (/=) ) $ sts `zip` tail sts where
  sts = iterate fun initial

visionLens :: a -> [[a]] -> [[Environ [a] a]]
visionLens e mat =
  let x = 42
  in undefined 

{-
Simulate your seating area by applying the seating rules repeatedly
until no seats change state. How many seats end up occupied?
-}
problem2 :: Board Seat -> Int
problem2 = occupied . fixpoint (map (map (evolveB . kili) . fili F) . rowZippers E )
