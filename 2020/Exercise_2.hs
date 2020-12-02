module Exercise2 where

{- Reference: https://adventofcode.com/2020/day/2
-}

{-
Each line gives the password policy and then the password. The password policy indicates the
lowest and highest number of times a given letter must appear for the password to be valid
-}
data Rule = Rule { char :: Char
                 , min:: Int
                 , max:: Int } deriving Eq

type Entry = (Rule , String)

--
-- tried to use a regex haskjell library but looks to be a bit of hassle...
-- Regex :: ^(\d+)-(\d+) (\l): (\l*)$
--
parseEntry :: String -> Entry
parseEntry entryStr =
  let [rule, ' ' : str] = splitOn ':' entryStr
      [bounds, [char]] = splitOn ' ' rule
      [inf, upp]  = splitOn '-' bounds
  in (Rule char (read inf) (read upp) , str)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn sep xs = pre : splitOn sep cutPost where
  (pre, post) = span (/= sep) xs
  cutPost = dropWhile (== sep) post


testParse = parseEntry "1-3 a: abcde" == (Rule 'a' 1 3, "abcde")


{- Part 2: we still have the Rules, as we used them above, but the check is different.
The min/max values only indicate positions in the string, starting with 1
-}

checkPasswordA :: Rule -> String -> Bool
checkPasswordA (Rule c inf upp) =
  between inf upp . count (== c) 

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

between :: Ord a => a -> a -> a -> Bool
between i u x = i <= x && x <= u

checkPasswordB :: Rule -> String -> Bool
checkPasswordB (Rule c inf upp) str =
  count (== c) [str !! (inf - 1), str !! (upp - 1)] == 1


runProblem :: (Rule -> String -> Bool) -> IO ()
runProblem checkPassword =
  do text <- readFile "input2.txt"
     let valid = count (uncurry checkPassword) . map parseEntry . lines $ text
     putStrLn (show valid)
