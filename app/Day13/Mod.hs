module Day13.Mod where

import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Utils.Coords
import Utils.Mod

data Fold = X Int | Y Int deriving (Show, Eq, Ord)

-- data Point = Filled | Empty deriving (Show, Eq, Ord)

type Dots = Set Coord

type PaperInfo = (Dots, [Fold])

toFold :: String -> Fold
toFold str = case end of
  ('y' : '=' : num) -> Y (read num)
  ('x' : '=' : num) -> X (read num)
  where
    end = fromJust $ stripPrefix "fold along " str

readCoord :: String -> Coord
readCoord str = C (read y) (read x)
  where
    [x, y] = splitOn "," str

getPaperInfo :: [String] -> PaperInfo
getPaperInfo strs = (dots, folds)
  where
    [cordStrs, foldStrs] = splitOn [""] strs
    cords = map readCoord cordStrs
    folds = map toFold foldStrs
    dots = S.fromList cords

readPaper :: IO PaperInfo
readPaper = getPaperInfo <$> readInputLines

getNewPos :: Int -> Int -> Int
getNewPos currPos foldPos = foldPos - (currPos - foldPos)

transposePoint :: Fold -> Coord -> Coord
transposePoint (Y line) c@(C y _) | y <= line = c
transposePoint (Y line) (C y x) = C (getNewPos y line) x
transposePoint (X line) c@(C _ x) | x <= line = c
transposePoint (X line) (C y x) = C y (getNewPos x line)

foldPaper :: Fold -> Dots -> Dots
foldPaper foldLine = S.map (transposePoint foldLine)

drawDots :: Dots -> String
drawDots dots = drawCoords' '.' asMap
  where
    asMap = M.fromList [(c, '#') | c <- S.elems dots]

printDots :: Dots -> IO ()
printDots dots = putStr $ drawDots dots

part1 :: IO ()
part1 = do
  (dots, folds) <- readPaper
  let foldedDots = foldPaper (head folds) dots
  printDots foldedDots
  print $ foldedDots
  print $ S.size foldedDots
  return ()

part2 :: IO ()
part2 = do
  (dots, folds) <- readPaper
  let foldedDots = foldl (flip foldPaper) dots folds
  printDots foldedDots
  print $ foldedDots
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]