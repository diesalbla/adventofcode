
module Day2 where

import Data.Functor((<&>))

data Outcome = Lose | Draw | Win  deriving (Eq, Show, Enum)

data Rps = Rock | Paper | Scissors deriving (Eq, Show, Enum)

fullSucc :: Rps -> Rps
fullSucc = shiftEnum 3 1

shiftRps :: Int -> Rps -> Rps
shiftRps n = shiftEnum 3 n 

{-- For a finite enumeration with "total" elements, cycle back (or forth)
 a number of `shift` positions from a given element. 
--}
shiftEnum :: Enum a => Int -> Int -> a -> a
shiftEnum total shift elem = toEnum $ (fromEnum elem + shift) `mod` total

{--
> Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.
> If both players choose the same shape, the round instead ends in a draw
--}
contest :: Rps -> Rps -> Outcome
contest  x y =
  if x == y then Draw
  else if y == fullSucc x then Lose
  else Win

{--
> The first column is [...]: A for Rock, B for Paper, and C for Scissors
--}

readRps :: Char -> Rps
readRps 'A' = Rock
readRps 'B' = Paper
readRps 'C' = Scissors


{--
: X for Rock, Y for Paper, and Z for Scissors
--}
decryptRps :: Char -> Rps
decryptRps 'X' = Rock
decryptRps 'Y' = Paper
decryptRps 'Z' = Scissors

parseLine1 :: String -> (Rps, Rps)
parseLine1 [x, ' ', y] = (readRps x, decryptRps y)

{--
 > The score for a single round is the score for the shape you selected
 > (1 for Rock, 2 for Paper, and 3 for Scissors) plus the score for the
 > outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
--}
outScore :: Outcome -> Int
outScore = (* 3) . fromEnum

rpsScore :: Rps -> Int
rpsScore = (+ 1) . fromEnum

scoreRight :: Rps -> Rps -> Int
scoreRight x y =
  let out = contest y x
  in outScore out + rpsScore y

testInput = ["A Y", "B X", "C Z"]
  
problem1 = sum . map (uncurry scoreRight . parseLine1)
-- problem1 = mapM_ (putStrLn . show . (uncurry scoreRight) . parseLine)


{--
> Anyway, the second column says how the round needs to end: X means you need to lose,
> Y means you need to end the round in a draw, and Z means you need to win. 
--}
decryptOut :: Char -> Outcome
decryptOut 'X' = Lose
decryptOut 'Y' = Draw
decryptOut 'Z' = Win


{-- achieve outcome opp 
if opponent plays `opp` and we want to achieve outcome,
what should we play? 
--}
achieve :: Outcome -> Rps -> Rps
achieve out x =
  let num = case out of
        Win -> 1
        Lose -> 2
        Draw -> 0
  in shiftRps num x

parseLine2 :: String -> (Rps, Rps)
parseLine2 [x, ' ', y] =
  let opp = readRps x
  in  (opp, achieve (decryptOut y) opp)


problem2 = sum . map (uncurry scoreRight . parseLine2)

solveProblem = readFile "input2.txt" <&> problem2 . lines -- >>= mapM_ putStrLn
