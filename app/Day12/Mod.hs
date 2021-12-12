{-# LANGUAGE TupleSections #-}

module Day12.Mod where

import Data.Char (isUpper)
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Debug.Trace
import Utils.Mod

data Cave = Big String | Small String deriving (Eq, Ord)

instance Show Cave where
  show (Big c) = c
  show (Small c) = c

type Edges = Map Cave [Cave]

startCave, endCave :: Cave
startCave = Small "start"
endCave = Small "end"

charToCave :: String -> Cave
charToCave c = if all isUpper c then Big c else Small c

toPair :: String -> (Cave, Cave)
toPair str = (x, y)
  where
    [x, y] = map charToCave $ splitOn "-" str

duplicatePairs :: [(Cave, Cave)] -> [(Cave, [Cave])]
duplicatePairs [] = []
duplicatePairs ((x, y) : xs) = (x, [y]) : (y, [x]) : duplicatePairs xs

getEdges :: IO Edges
getEdges = M.fromListWith (++) . duplicatePairs . map toPair <$> readInputLines

type Path = [Cave]

smallIsMember :: Cave -> Path -> Bool
smallIsMember (Big _) _ = True
smallIsMember cave@(Small _) path = cave `notElem` path

-- Remove all small caves we have visited in path
filterVisitedSmall :: Path -> [Cave] -> [Cave]
filterVisitedSmall currPath = filter (`smallIsMember` currPath)

edgeLookup :: Edges -> Cave -> [Cave]
edgeLookup edges curr = M.findWithDefault [] curr edges

getPaths :: Edges -> Cave -> Path -> [Path]
getPaths edges currCave currPath | currCave == endCave = [currCave : currPath]
getPaths edges currCave currPath = concatMap followPath filterPossible
  where
    addCurrent = currCave : currPath
    possibleCaves = edgeLookup edges currCave
    filterPossible = filterVisitedSmall currPath possibleCaves -- traceShow (currCave, possibleCaves) $
    followPath newCave = getPaths edges newCave addCurrent

getPathsFromStart :: Edges -> [Path]
getPathsFromStart edges = getPaths edges startCave []

printPath :: Path -> String
printPath p = unwords $ map show $ reverse p

printPaths :: [Path] -> IO ()
printPaths paths = putStr $ unlines $ sort (map printPath paths)

part1 :: IO ()
part1 = do
  input <- getEdges
  let paths = getPathsFromStart input
  printPaths paths
  print $ length paths
  return ()

isCaveAllowed :: Cave -> Path -> Bool -> Maybe (Cave, Bool) -- If just, cave is allowed the bool if is there is a second small
isCaveAllowed cave@(Big _) _ aSmallHas2 = Just (cave, aSmallHas2)
isCaveAllowed (Small "start") _ _ = Nothing
isCaveAllowed cave@(Small _) path aSmallHas2 = case (thisCount, aSmallHas2) of
  (x, _) | x < 1 -> Just (cave, aSmallHas2)
  (1, False) -> Just (cave, True)
  (_, _) -> Nothing
  where
    thisCount = count (cave ==) path

-- isCavedAllowedMaybe :: Cave -> Path -> Bool -> Just Bool

countPathsPart2 :: Edges -> [(Cave, Path, Bool)] -> Int
countPathsPart2 _ [] = 0
countPathsPart2 e ((cave, _, _) : xs) | cave == endCave = 1 + countPathsPart2 e xs
countPathsPart2 edges ((currCave, currPath, has2Small) : xs) = countPathsPart2 edges nextElems
  where
    neighbors = edgeLookup edges currCave
    possible = mapMaybe (\cave -> isCaveAllowed cave currPath has2Small) neighbors
    nextElems = map (\(cave, newHas2Small) -> (cave, currCave : currPath, newHas2Small)) possible ++ xs

part2 :: IO ()
part2 = do
  input <- getEdges
  print $ countPathsPart2 input [(startCave, [], False)]
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]