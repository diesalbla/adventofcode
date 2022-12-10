module Day7 where

import Data.Char(isDigit)
import Data.Functor((<&>))
import Data.List(isPrefixOf)
import qualified Data.Map as M

data FileTree = File Int | Dir (M.Map String FileTree) deriving (Eq, Show)

file :: Int -> FileTree
file  = File

dir :: [(String, FileTree)] -> FileTree
dir = Dir . M.fromList

testFileTree :: FileTree =
  dir [ ("a", dir [
            ("e", dir [ ("i", file 584) ] ),
            ("f", file 29116),
            ("g", file 2557),
            ("h.lst", file 62596)
            ]),
        ("b.txt", file 14848514),
        ("c.dat", file 8504156),
        ("d", dir [
            ("j", file 4060174),
            ("d.log", file 8033020),
            ("d.ext", file 5626152),
            ("k", file 7214296)
            ])
      ]

printFileTree :: FileTree -> [String]
printFileTree = printAux 0 "/" where
  printAux n name (File size) = [fileLine n name size]
  printAux n name (Dir files) = dirLine : dirElems where
    dirLine = concat [indent n, "- ", name, " (dir)"]
    dirElems = M.assocs files >>= uncurry (printAux (n+ 1)) 
  fileLine n name size = concat [indent n, "- ", name, " (file, size=", show size, ")"]
  indent n = take (2 * n) $ repeat ' '

subDirs :: FileTree -> [FileTree]
subDirs (File _) = []
subDirs dd@(Dir mm) = dd : (M.elems mm >>= subDirs)

totalSize :: FileTree -> Int
totalSize (File n) = n
totalSize (Dir mm) = sum . map totalSize . M.elems $ mm

{-- Bigger problem: the parsing.
> Within the terminal output, lines that begin with $ are commands you executed, very much like some modern computers:


How do we build our fileSystem? Well, we are exploring it, so we need to keep a track
- We need to keep a stack of directories we are "charting" and a file-path we are at

- Initial state: empty path, but with empty directory 

--}

data ChartState = ChartState { current :: FileTree, stack ::  [(M.Map String FileTree, String)] }

initChartState :: ChartState
initChartState = ChartState (dir []) []

isRoot :: ChartState -> Bool
isRoot = null . stack 

{-- Change of state: --}
dirDown :: String -> ChartState -> ChartState
dirDown dirName (ChartState (Dir curDir) stack) =
  let newDir = case M.lookup dirName curDir of
        Nothing -> dir []
        Just (File _)  -> error $ "directory Name " ++ dirName ++ "is registered as file"
        Just dd@(Dir _) -> dd
  in ChartState newDir ((curDir, dirName) : stack)


{-- Change of State: we go up the directory and we need to record our findings
--}
dirUp :: ChartState -> ChartState
dirUp (ChartState curDir ((prevDir, path) : stack) ) =
  let newDir = Dir $ M.insert path curDir prevDir 
  in ChartState newDir stack

dirRoot :: ChartState -> ChartState
dirRoot cs = if isRoot cs then cs else dirRoot (dirUp cs)

addFile :: String -> Int -> ChartState -> ChartState
addFile name size (ChartState (Dir curDir) stack) =
  let newDir = Dir $ M.insert name (File size) curDir
  in  ChartState newDir stack

isFileLine :: String -> Bool
isFileLine = not . null . takeWhile isDigit

runCommand :: ChartState -> String -> ChartState
runCommand cs str
  | "$ cd /" `isPrefixOf` str  = dirRoot cs
  | "$ cd .." `isPrefixOf` str = dirUp cs
  | "$ cd " `isPrefixOf` str   = dirDown (drop 5 str) cs
  | "$ ls" `isPrefixOf` str    = cs
  | "dir " `isPrefixOf` str    = cs
  | isFileLine str  =
      let (digs, ' ' : name) = span isDigit str
      in addFile name (read digs) cs

parseFileTree :: String -> FileTree
parseFileTree =  current . dirRoot . foldl runCommand initChartState . lines

problem1 :: FileTree -> Int
problem1 = sum . filter (<= 100_000) . map totalSize . subDirs

{--
> Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update.
> What is the total size of that directory?
--}
excessSize :: FileTree -> Int
excessSize = (+ (- 40_000_000)) . totalSize

problem2 :: FileTree -> Int
problem2 ft = minimum . filter (>= excessSize ft) . map totalSize . subDirs $ ft

solveProblem = readFile "input7.txt" <&> problem2  . parseFileTree

