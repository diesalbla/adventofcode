module Exercise14 where

import Control.Applicative((<|>), liftA2)
import Data.Char(isDigit)
import Data.Functor((<&>))
import Data.Maybe(fromMaybe, catMaybes)
import qualified Data.Map as M
import Data.List(unfoldr, isPrefixOf, stripPrefix)


data Bit = Z | O deriving (Eq, Show)

{- The bitmask is always given as a string of 36 bits,
written with the most significant bit (representing 2^35)
on the left and the least significant bit (2^0, that is, the 1s bit)
on the right
-}
type BWord = [Bit]

bword2int :: BWord -> Integer
bword2int = foldl (\ acc x -> acc * 2 + bitInt x) 0 where
  bitInt Z = 0
  bitInt O = 1

int2bword :: Integer -> BWord
int2bword = reverse . take 36 . unfoldr aux where
  aux n = let (q, r) = n `quotRem` 2 in Just (toBit r, q)
  toBit 0 = Z
  toBit 1 = O

data Mask = Mask { mbits :: [Maybe Bit] } deriving (Eq)

instance Show Mask  where
  show (Mask mbits) = "mask := " ++ concatMap (maybe "_" show) mbits

parseMBit :: Char -> Maybe Bit
parseMBit '1' = Just O
parseMBit '0' = Just Z
parseMBit _   = Nothing

maskWith :: Mask -> BWord -> BWord
maskWith (Mask mbits) = zipWith (flip fromMaybe) mbits

data Instr = Assign { memAddr :: Integer , value :: Integer }
           | ChangeMask { newMask :: Mask }
           deriving (Eq, Show)

parseInstr :: String -> Maybe Instr
parseInstr str = parseAssign str <|> parseChangeMask str

-- assign:   'mem['` (\d+) '] = '(\d+)`
parseAssign :: String -> Maybe Instr
parseAssign str =
  do s1 <- stripPrefix "mem[" str
     let (addrStr, rest) = span isDigit s1
     s2 <- stripPrefix "] = " rest
     pure $ Assign (read addrStr) (read s2)

parseChangeMask :: String -> Maybe Instr
parseChangeMask  = fmap (ChangeMask . Mask . map parseMBit) . stripPrefix "mask = "

data State = State { memo :: M.Map Integer Integer
                   , mask :: Mask
                   } deriving (Eq, Show)

initState = State M.empty (Mask [] )

step :: Instr -> State -> State
step (ChangeMask nmask) st = st { mask = nmask }
step (Assign addr nval) (State mem oldMask) = State nmemo oldMask where
  nmemo = M.insert addr maskedVal mem
  maskedVal = bword2int . maskWith oldMask . int2bword $ nval


problem1 :: [Instr] -> Integer
problem1 = sum . M.elems . memo . foldl (flip step) initState

parseInput :: IO [Instr]
parseInput = readFile "input14.txt" <&> ( catMaybes . map parseInstr . filter (not . null) . lines)

parseFile :: String -> IO [Instr]
parseFile name = readFile name <&> ( catMaybes . map parseInstr . filter (not . null) . lines)


-- Part 2: the mask is not an "overwrite or leave" but "leave, overwrite to zero, "

{-

> If the bitmask bit is 0, the corresponding memory address bit is unchanged.
> If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
> If the bitmask bit is X, the corresponding memory address bit is floating.

-}
maskGenerate :: [Maybe Bit] -> BWord -> [BWord]
maskGenerate [] [] = [[]]
maskGenerate (mbit:mbits) (bit:bits) =
  liftA2 (:) (floatBit bit mbit) (maskGenerate mbits bits)

floatBit :: Bit -> Maybe Bit -> [Bit]
floatBit b (Just Z) = [b]
floatBit _ (Just O) = [O]
floatBit _ Nothing  = [Z, O]


-- NOTE: The above looks like a Traverse on the Applicative instance of lists.


stepB :: State -> Instr -> State
stepB st (ChangeMask nmask) = st {mask = nmask}
stepB st (Assign addr nval) = st { memo = nmemo } where
  nmemo = (`M.union` memo st)
          . M.fromList
          . map (\x -> (x, nval))
          . map bword2int
          . maskGenerate (mbits . mask $ st)
          . int2bword
          $ addr

problem2 :: [Instr] -> Integer
problem2 = sum . M.elems . memo .foldl stepB initState
