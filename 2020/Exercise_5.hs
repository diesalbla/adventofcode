
module Exercise5 where


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
findMissing boards =
  let inf = minimum boards
      upp = maximum boards
      good i = map (`elem` boards) [i-1, i, i+1] == [True, False, True]
  in  head . filter good $ [inf .. upp]
  
runProblem =
  readFile "input5.txt" >>= (putStrLn . show . findMissing . map parseBoard . lines)


  
  
