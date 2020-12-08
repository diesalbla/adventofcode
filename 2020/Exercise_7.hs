module Exercise7 where

import Control.Applicative((<|>))
import Data.List(isPrefixOf, stripPrefix)
import Data.Functor((<&>))
import qualified Data.Maybe as Mb
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char(isDigit)
import Util

type Color = String
data Rule = Rule { outer :: Color
                 , inner :: [(Color, Int)]
                 } deriving (Eq, Show)

-- Function: given separator word (a list) split prefix/suffix of it.
-- At this point I could learn to use a bloody parsec...

breakAt :: Eq a => [a] -> [a] -> Maybe ([a], [a])
breakAt sep [] = Nothing
breakAt sep xs @ (y:ys) =
  case stripPrefix sep xs of
    Nothing -> fmap (attach y) (breakAt sep ys)
    Just xs -> Just ([], xs)
  where attach x (pre, post) = (x: pre, post)

-- breakMany: break many by the separator
breakMany :: Eq a => [a] -> [a] -> [[a]]
breakMany sep [] = []
breakMany sep xs =
  case breakAt sep xs of
    Nothing -> [xs]
    Just (pre, post) -> pre: breakMany sep post

parseInner :: String -> Maybe (Color, Int)
parseInner str =
  do noBags <- (stripSuffix " bags" str <|> stripSuffix " bag" str)
     let (num, ' ': color) = span isDigit noBags
     pure $ (color, read num)

parseRule :: String -> Maybe Rule
parseRule str =
  do (col, inn) <- breakAt " bags contain " str
     noDot      <- stripSuffix "." inn
     contents   <- noOtherBags noDot <|> parseContents noDot
     pure       $  Rule col contents
  where
    noOtherBags   = fmap (\_ -> []) . stripSuffix "no other bags"
    parseContents = traverse parseInner . breakMany ", "


{-
You have a shiny gold bag. If you wanted to carry it in at least one
other bag, how many different bag colors would be valid for the
outermost bag? (In other words: how many colors can, eventually,
contain at least one shiny gold bag?)
-}
problem1 :: [Rule] -> Int
problem1 rules =
  let containMap = map adjacency rules
      initState =  (S.singleton "shiny gold", S.empty)
  in length . snd . head . dropWhile (not . S.null . fst) $ iterate (expand containMap) initState

adjacency :: Rule -> (Color, [Color])
adjacency (Rule from tos) = (from, map fst tos)

expand :: Ord a => [(a, [a])] -> (S.Set a, S.Set a) -> (S.Set a, S.Set a)
expand containMap (frontier, reached) =  (nfrontier, reached `S.union` frontier) where
  nfrontier     = flip S.difference reached . unions . map containers . S.toList $ frontier
  containers cl = S.fromList . map fst . filter (elem cl . snd) $ containMap 
  unions        = foldr S.union S.empty

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suff = fmap reverse . stripPrefix (reverse suff) . reverse

test1 = parseRule text == Just expected where
  text     = "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  expected = Rule "dark orange" [("bright white", 3), ("muted yellow", 4)]

test2 = parseRule text == Just expected where
  text = "dark crimson bags contain 1 striped chartreuse bag, 4 dark tan bags, 2 faded fuchsia bags, 5 mirrored black bags."
  expected = Rule "dark crimson" [ ("striped chartreuse", 1)
                                 , ("dark tan", 4)
                                 , ("faded fuchsia", 2)
                                 , ("mirrored black", 5)
                                 ]

tally :: M.Map Color [(Color, Int)] -> ([(Color, Int)], Int) -> ([(Color, Int)], Int)
tally rules (frontier, acc) = (nfrontier, acc + sum (map snd frontier)) where
  nfrontier = do (cl, n) <- frontier
                 (k, m)  <- (rules M.! cl)
                 pure (k, m*n)
                              
problem2 :: [Rule] -> Int
problem2 rules =
  let require = M.fromList . map (\ (Rule c ps) -> (c, ps)) $ rules
      initState = ([("shiny gold", 1)], 0)
  in  snd . head . dropWhile (not . null . fst) . iterate (tally require) $ initState
      
parseRulesFile :: IO [Rule]
parseRulesFile = readFile "input7.txt"
                  <&> (Mb.catMaybes . map parseRule . filter (not . null) . lines)

runProblemA = parseRulesFile <&> problem1 >>= (putStrLn . show)
runProblemB = parseRulesFile <&> problem2 >>= (putStrLn . show)

    
examplePart2 =
  let rules = [ Rule "shiny gold" [("dark red", 2)]
              , Rule "dark red"   [("dark orange", 2)]
              , Rule "dark orange" [("dark yellow", 2)]
              , Rule "dark yellow" [("dark green", 2)]
              , Rule "dark green"  [("dark blue", 2)]
              , Rule "dark blue"   [("dark violet", 2)]
              , Rule "dark violet" []
              ]
  in problem2 rules - 1 == 126

              
