
module Day2 where

import qualified Data.Map as M
import Data.Maybe(catMaybes)
import Data.Functor((<&>))
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Char as PC
import Text.Parsec.Language(haskellDef)

type Colour = String


data Game = Game { idCode :: Integer
                 , sets :: [M.Map Colour Integer]
                 } deriving (Eq, Show)

-- PARSING THE INPUT
gameParse :: GenParser Char st Game
gameParse = do
  PC.string "Game" >> PC.spaces 
  ident <- PT.decimal lexer
  PC.char ':' 
  sets <- PT.semiSep lexer setParse
  pure $ Game ident sets

setParse :: GenParser Char st (M.Map Colour Integer)
setParse = PT.commaSep lexer (PC.spaces >> pairParse)  <&> M.fromList 

pairParse :: GenParser Char st (Colour, Integer)
pairParse = do
  PC.spaces
  n <- PT.decimal lexer
  PC.spaces
  s <- PT.identifier lexer
  pure $ (s, n)

parseGame :: String -> Game
parseGame = either (const undefined) id . parse gameParse "" 

lexer = PT.makeTokenParser Text.Parsec.Language.haskellDef

part1 :: [Game] -> Integer
part1 = sum . map idCode . filter isPossible  where
  bag = M.fromList [ ("red", 12), ("green", 13), ("blue", 14)]
  isPossible = all (\s -> M.isSubmapOfBy (<=) s bag) . sets


{-- The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. --}
part2 :: [Game] -> Integer
part2 = sum . map minPower where 
  minPower = product . foldr (unionBy max) M.empty . sets
  unionBy :: Ord k => (a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a
  unionBy f m1 = foldr (uncurry (M.insertWith f)) m1 . M.assocs

main1 =  readFile "input2.txt" <&>  part1 . catMaybes . map parseGame . lines
main2 =  readFile "input2.txt" <&>  part2 . catMaybes . map parseGame . lines
