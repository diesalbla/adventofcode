module Exercise5 where
import Data.Functor((<&>))
import Data.List(tails, sort)

bit :: Char -> Int
bit 'F' = 0
bit 'B' = 1
bit 'L' = 0
bit 'R' = 1

parseBoard :: String -> Int
parseBoard board =
  let (rowS, colS) = span (`elem` "FB") board
      addTot num acc = acc * 2 + num
      row = foldr addTot 0 . reverse . map bit $ rowS
      col = foldr addTot 0 . reverse . map bit $ colS
  in  row * 8 + col

test1 = parseBoard "FBFBBFFRLR" == 357

findMissing :: [Int] -> Int
findMissing =
  (+1)
  . head
  . head
  . filter (\ (x:y:_) -> y /= x+1 )
  . filter ( (== 2) . length . take 2)
  . tails
  . sort 
    
runProblem = readFile "input5.txt"
  <&> show . findMissing . map parseBoard . lines
  >>= putStrLn


  
  
