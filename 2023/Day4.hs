module Day4 where

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Char as PC
import Text.Parsec(sepBy)
import Text.Parsec.Language(haskellDef)

data Card = Card { idCode :: Int, winning :: [Int], hand :: [Int] } deriving Show

-- PARSER
cardParse :: GenParser Char st Card
cardParse = do
  PC.string "Card"
  PC.spaces 
  ide <- number
  PC.char ':'
  PC.spaces
  ws <- many (number <* spaces)
  PC.char '|'
  PC.spaces
  hs <- many (number <* spaces)
  pure $ Card (fromInteger ide) (map fromInteger ws) (map fromInteger hs)

lexer = PT.makeTokenParser Text.Parsec.Language.haskellDef

forceParse p = fromRight . parse p ""

fromRight :: Either a b -> b
fromRight = either (const undefined) id

number = PT.decimal lexer

countWinners :: Card -> Int 
countWinners c = length . filter (`elem` winning c) $ hand c

points n = if n <= 0 then 0 else 2 ^ (n-1)

part1 :: [String] -> Int
part1 = sum . map (points . countWinners . forceParse cardParse)

type Jet = [Int] -- Invariant : only positive numbers

waveStep :: [Int] -> Int -> ([Int], Int)
waveStep jet num = (newJet ++ rest, numCards) where
  rest = filter (>0) . map (\x -> x - 1) $ jet
  numCards = length jet + 1
  newJet = if num == 0 then [] else replicate numCards num

parapara :: (b -> a -> (b, c)) -> b -> [a] -> [c]
parapara _ _ [] = []
parapara fun acc (x: xs) = (y : parapara fun nacc xs) where (nacc, y) = fun acc x

part2 = sum . parapara waveStep [] . map (countWinners . forceParse cardParse)
