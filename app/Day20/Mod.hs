module Day20.Mod where

import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Utils.Coords
import Utils.Mod

data Pixel = Light | Dark deriving (Eq, Show)

showPixel :: Pixel -> Char
showPixel Light = '#'
showPixel Dark = '.'

toPixel :: Char -> Pixel
toPixel '#' = Light
toPixel '.' = Dark
toPixel c = error ("invalid char:" ++ [c])

pixelToDigit :: Pixel -> Int
pixelToDigit Light = 1
pixelToDigit Dark = 0

type Image = Map Coord Pixel

type ImageAlg = [Pixel]

getInput :: [String] -> (Image, ImageAlg)
getInput strs = (imageMap, imageAlg)
  where
    deepToPixel x = map (map toPixel) x
    [alg, imageStr] = splitOn [""] strs
    imageMap = M.fromList $ coordLines $ deepToPixel imageStr
    imageAlg = concat $ deepToPixel alg

readImageInfo :: IO (Image, ImageAlg)
readImageInfo = getInput <$> readInputLines

displayImage :: Image -> String
displayImage = drawCoordsGen showPixel Dark

-- | Returns 9 Coords around and including c in order from top left to bottom right
convolutionCoords :: Coord -> [Coord]
convolutionCoords c = sort $ c : neighbors c

getPixel :: Image -> Pixel -> Coord -> Pixel
getPixel iMap def c = M.findWithDefault def c iMap

getConvolutionPixel :: Pixel -> Image -> ImageAlg -> Coord -> Pixel
getConvolutionPixel def iMap alg c = alg !! lookupIdx
  where
    coords = convolutionCoords c
    lookupIdx = toDecimal $ map (pixelToDigit . getPixel iMap def) coords

--- | Account for infinite image so get all coords in image + 3 on each side
getImageBounds :: Image -> [Coord]
getImageBounds iMap = [C y x | x <- getRange minx maxx, y <- getRange miny maxy]
  where
    Just (C miny minx, C maxy maxx) = boundingBox (M.keys iMap)
    getRange min max = [min -3 .. max + 3]

runStep :: Pixel -> ImageAlg -> Image -> Image
runStep def alg iMap = M.fromList newPixels
  where
    coords = getImageBounds iMap
    newPixels = [(c, getConvolutionPixel def iMap alg c) | c <- coords]

runNSteps :: Int -> [Pixel] -> ImageAlg -> Image -> Image
-- runNSteps n alg iMap = iterate (runStep alg) iMap !! n
runNSteps 0 _ _ iMap = iMap
runNSteps n defPixels alg iMap = runNSteps (n -1) defPixels alg (runStep (defPixels !! n) alg iMap)

getDefaults :: ImageAlg -> [Pixel]
getDefaults [] = error "empty"
getDefaults (Dark : xs) = repeat Dark
getDefaults x = Dark : cycle [head x, last x]

part1 :: IO ()
part1 = do
  (image, alg) <- readImageInfo
  let newImage = runNSteps 2 (getDefaults alg) alg image
  putStr $ displayImage newImage
  print $ count (== Light) newImage
  -- print "part1"
  return ()

part2 :: IO ()
part2 = do
  (image, alg) <- readImageInfo
  let newImage = runNSteps 50 (getDefaults alg) alg image
  -- putStr $ displayImage newImage
  print $ count (== Light) newImage
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]