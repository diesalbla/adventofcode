module Exercise4 where

import Data.List(lookup)
import Data.Maybe(catMaybes)
import Data.Char(isDigit, isHexDigit)

{-
> The automatic passport scanners are slow because they're having trouble detecting
> which passports have all required fields. The expected fields are as follows:
>
>    byr (Birth Year)
>    iyr (Issue Year)
>    eyr (Expiration Year)
>    hgt (Height)
>    hcl (Hair Color)
>    ecl (Eye Color)
>    pid (Passport ID)
>    cid (Country ID)
-}
data Passport =
  Passport { birthYear :: Int
           , issueYear :: Int
           , expirationYear :: Int
           , height :: String
           , hairColor :: String
           , eyeColor :: String
           , passportId :: String
           , countryId :: Maybe String
           } deriving (Eq, Show)

parsePassport :: [(String, String)] -> Maybe Passport
parsePassport ps = 
  let byr = fmap read $ lookup "byr" ps
      iyr = fmap read $ lookup "iyr" ps
      eyr = fmap read $ lookup "eyr" ps
      hgt = lookup "hgt" ps
      hcl = lookup "hcl" ps
      ecl = lookup "ecl" ps
      pid = lookup "pid" ps
      cid = pure (lookup "cid" ps)
  in pure Passport <*> byr <*> iyr <*> eyr <*> hgt <*> hcl <*> ecl <*> pid <*> cid 

splitAtElem :: Eq a => a -> [a] -> [[a]]
splitAtElem _ [] = []
splitAtElem e as =
  let (pre, suff) = span (/= e) as
      pref = if null pre then [] else [pre]
      sufx = splitAtElem e (dropWhile (== e) suff)
  in  pref ++ sufx

parseChunk :: [String] -> Maybe Passport
parseChunk = parsePassport . map splitPair. concatMap (splitAtElem ' ')
  where
    splitPair s = let (xs, ':': ys) = span (/= ':') s in (xs, ys)

testChunk :: [String]
testChunk =
  ["hcl:#ae17e1 iyr:2013",
  "eyr:2024",
  "ecl:brn pid:760753108 byr:1931",
  "hgt:179cm"]

breakupText :: [String] -> [Passport]
breakupText = catMaybes . map parseChunk . splitAtElem ""

runProblem =
  readFile "input4.txt" >>= (putStrLn . show . length . filter checkPassword . breakupText . lines)

{-
> You can continue to ignore the cid field, but each other field
> has strict rules about what values are valid for automatic validation:
>     byr (Birth Year) - four digits; at least 1920 and at most 2002.
>     iyr (Issue Year) - four digits; at least 2010 and at most 2020.
>     eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
>     hgt (Height) - a number followed by either cm or in:
>         If cm, the number must be at least 150 and at most 193.
>         If in, the number must be at least 59 and at most 76.
>     hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
>     ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
>     pid (Passport ID) - a nine-digit number, including leading zeroes.
>     cid (Country ID) - ignored, missing or not.
-}
checkPassword :: Passport -> Bool
checkPassword (Passport byr iyr eyr hgt hcl ecl pid _) =
  and [ byr >= 1920 && byr <= 2002,
        iyr >= 2010 && iyr <= 2020,
        eyr >= 2020 && eyr <= 2030,
        checkHeight hgt,
        checkHairColor hcl,
        elem ecl ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"],
        length pid == 9 && all isDigit pid
      ]

checkHeight :: String -> Bool
checkHeight hgt =
  let (digs, un) = span isDigit hgt
      num = read digs
  in case un of
       "cm" -> num >= 150 && num <= 193
       "in" -> num >= 59 && num <= 76
       _    -> False

checkHairColor :: String -> Bool
checkHairColor str = length str == 7 && head str == '#' && all isHexDigit (tail str)
  
testInput :: [String]
testInput =
    [
      "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
      "byr:1937 iyr:2017 cid:147 hgt:183cm",
      "",
      "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
      "hcl:#cfa07d byr:1929",
      "",
      "hcl:#ae17e1 iyr:2013",
      "eyr:2024",
      "ecl:brn pid:760753108 byr:1931",
      "hgt:179cm",
      "",
      "hcl:#cfa07d eyr:2025 pid:166559648",
      "iyr:2011 ecl:brn hgt:59in"
    ]

