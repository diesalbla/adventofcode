

module Day13 where

import Data.Function(on)

import Text.ParserCombinators.Parsec

data Packet = PInt Int | PList [Packet] deriving Eq

splitBlocks :: [String] -> [[String]]
splitBlocks [] = []
splitBlocks xs =
  let (pre, post) = span (not . null) xs
  in  pre : splitBlocks (drop 1 post)

-- parsePacket :: String -> (Packet
-- parsePacket "" = PList []
-- parsePacket '[' : str =
--   let (p, rest) = parseAux str
-- 
packet :: GenParser Char st Packet
packet =
  
  <|>

listPacket :: GenParser Char st Packet
listPacket = (char '[') >> 


parsePacket :: String -> Either ParseError [[String]]
parsePacket = parse packet "(unknown)" 


parsePair :: [String] -> (Packet, Packet)
parsePair [x, y] = (parsePacket x, parsePacket y)

flatten :: Packet -> [Int]
flatten (PInt n) = [n]
flatten (PList ps) = ps >>= flatten

instance Ord Packet where
  compare = compare `on` flatten

diffIndex :: [Int] -> [Int] -> Int
diffIndex xs ys = length . takeWhile (uncurry (==)) $ xs `zip` ys

problem1 = sum
  . map fst
  . filter ((uncurry (<=) . snd))
  . zip [1..]
  . map parsePair
  . splitBlocks
  . lines

solveProblem = readFile "input13.txt" <&> problem1 

