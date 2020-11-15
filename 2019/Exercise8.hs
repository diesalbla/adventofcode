module Exercise8 where

import Control.Applicative
import Data.Function(on)
import Data.List(minimumBy)
import Data.Maybe

{-
Images are sent as a series of digits that each represent the color of a single pixel.
The digits fill each row of the image left-to-right, then move downward to the next row,
filling rows top-to-bottom until every pixel of the image is filled.
-}
type Layer a = [[a]]
type Image a = [[[a]]]

splitImage :: Int -> Int -> [a] -> Image a
splitImage width height = takeIter height . takeIter width where
  takeIter :: Int -> [a] -> Layer a
  takeIter n xs = if null xs then [] else take n xs : takeIter n (drop n xs)

{-
[...]] would like you to find the layer that contains the fewest 0 digits.
On that layer, what is the number of 1 digits multiplied by the number of 2 digits?
-}
checkSum :: [Layer Char] -> Int
checkSum = csum . minimumBy (compare `on` layerCount '0') where 
  csum layer = layerCount '1' layer * layerCount '2' layer
  layerCount :: Eq a => a -> Layer a -> Int
  layerCount x = sum . map (length . filter (x ==))

problemA :: Int -> Int -> String -> Int
problemA width height = checkSum . splitImage width height 

{--
The image is rendered by stacking the layers and
aligning the pixels with the same positions in each layer.
The digits indicate the color of the corresponding pixel:
0 is black, 1 is white, and 2 is transparent.

The layers are rendered with the first layer in front and the last layer in back.
So, if a given position has a transparent pixel in the first and second layers,
a black pixel in the third layer, and a white pixel in the fourth layer,
the final image would have a black pixel at that position.

render :: Width, Length, raw image to rendered image
-}
render :: [[String]] -> [String]
render = foldl1 (zipWith (zipWith addPix))
  where addPix acc c = if acc == '2' then c else acc

test2 = render (splitImage 2 2 "0222112222120000") == ["01", "10"]


{-- What message is produced after decoding your image? -}
problem2 = mapM_ (putStrLn . map prettyChar) . render . splitImage 25 6 $ input where
  prettyChar '0' = ' '
  prettyChar '1' = '*'
  input = undefined 

