module Day05.Mod where

import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Utils.Coords
import Utils.Mod

-- Start and end of the line
type Line = (Coord, Coord)

-- | Map of coord to how many lines hit that point
type CoveredPoints = Map Coord Int

readPair :: String -> Coord
readPair str = C (read y) (read x)
  where
    [x, y] = splitOn "," str

toLine :: String -> Line
toLine str = (readPair start, readPair end)
  where
    [start, end] = splitOn " -> " str

isHorizontalLine :: Line -> Bool
isHorizontalLine (C y1 _, C y2 _) = y1 == y2

isVertLine :: Line -> Bool
isVertLine (C _ x1, C _ x2) = x1 == x2

isSouthEastDiag, isSouthWestDiag, isNorthWestDiag, isNorthEastDiag :: Line -> Bool
isSouthEastDiag (start, end) = (start `isAbove` end) && (start `isLeft` end)
isSouthWestDiag (start, end) = (start `isAbove` end) && (start `isRight` end)
isNorthWestDiag (start, end) = (start `isBelow` end) && (start `isRight` end)
isNorthEastDiag (start, end) = (start `isBelow` end) && (start `isLeft` end)

isDiag :: Line -> Bool
isDiag line = any (\func -> func line) [isSouthEastDiag, isSouthWestDiag, isNorthWestDiag, isNorthEastDiag]

followLine :: Coord -> Coord -> DirFunc -> CoveredPoints -> CoveredPoints
followLine currCoord endCoord dirFunc points =
  if currCoord == endCoord
    then updatedPoints
    else followLine nextCoord endCoord dirFunc updatedPoints
  where
    updatedPoints = Map.insertWith (+) currCoord 1 points
    nextCoord = dirFunc currCoord

joinCoveredPoints :: [CoveredPoints] -> CoveredPoints
joinCoveredPoints = foldl (Map.unionWith (+)) Map.empty

countPointsGreaterThan :: Int -> CoveredPoints -> Int
countPointsGreaterThan val points = count (>= val) (Map.elems points)

coverLine :: Line -> CoveredPoints
coverLine line@(start, end) = followLine start end (lineToDir line) Map.empty

buildPoints :: [Line] -> CoveredPoints
buildPoints lines = joinCoveredPoints (map coverLine lines)

part1 :: IO ()
part1 = do
  input <- map toLine <$> readInputLines
  let points = buildPoints (filter (not . isDiag) input)
  print $ countPointsGreaterThan 2 points
  return ()

lineToDir :: Line -> DirFunc
lineToDir line@(start, end)
  | isSouthEastDiag line = southEast
  | isSouthWestDiag line = southWest
  | isNorthEastDiag line = northEast
  | isNorthWestDiag line = northWest
  | isAbove start end = below
  | isBelow start end = above
  | isLeft start end = right
  | isRight start end = left
  | otherwise = error "bad line"

part2 :: IO ()
part2 = do
  input <- map toLine <$> readInputLines
  let points = buildPoints input
  print $ countPointsGreaterThan 2 points
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]