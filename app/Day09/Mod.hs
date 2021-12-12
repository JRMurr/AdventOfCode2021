{-# LANGUAGE TupleSections #-}

module Day09.Mod where

import Data.Char (digitToInt)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import Utils.Coords
import Utils.Mod

type Tubes = Map Coord Int

isLowPoint :: Tubes -> Coord -> Bool
isLowPoint tubes c = all (currVal <) neighborVals
  where
    neighborCoords = neighborsCardinal c
    neighborVals = mapMaybe (`Map.lookup` tubes) neighborCoords
    currVal = tubes Map.! c

getLowPointsCoords :: Tubes -> [Coord]
getLowPointsCoords tubes = Map.keys $ Map.filterWithKey (\coord _ -> isLowPoint tubes coord) tubes

getLowPoints :: Tubes -> [Int]
getLowPoints tubes = map (1 +) $ Map.elems lowPointsMap
  where
    lowPointsMap = Map.filterWithKey (\coord _ -> isLowPoint tubes coord) tubes

part1 :: IO ()
part1 = do
  input <- Map.fromList . coordLinesInt . map (map digitToInt) <$> readInputLines
  print $ sum $ getLowPoints input
  return ()

type Basin = [Coord]

getBasinSizes :: BasinMap -> [Int]
getBasinSizes bMap = map length $ Map.elems bMap

-- Map of lowPoint cord to coords in its basin
type BasinMap = Map Coord Basin

isCoordValid :: Tubes -> Coord -> Bool
isCoordValid tubes c | c `Map.notMember` tubes = False
isCoordValid tubes c = tubes Map.! c < 9

isCordInBasin :: BasinMap -> Coord -> Coord -> Bool
isCordInBasin bMap lowPoint c = c `elem` currBasins
  where
    currBasins = Map.findWithDefault [] lowPoint bMap

fillBasins :: Tubes -> BasinMap -> [(Coord, Coord)] -> BasinMap
fillBasins _ bMap [] = bMap
fillBasins tubes bMap ((lowPoint, possiblePoint) : xs) | isCordInBasin bMap lowPoint possiblePoint = fillBasins tubes bMap xs
fillBasins tubes bMap ((lowPoint, possiblePoint) : xs) = fillBasins tubes (addCurToMap) (newToVisit ++ xs)
  where
    addCurToMap = Map.adjust (possiblePoint :) lowPoint bMap
    validNeighbors = filter (isCoordValid tubes) (neighborsCardinal possiblePoint)
    newToVisit = map (lowPoint,) validNeighbors

initBmap :: [Coord] -> BasinMap
initBmap lowPoints = Map.fromList [(l, []) | l <- lowPoints]

part2 :: IO ()
part2 = do
  input <- Map.fromList . coordLinesInt . map (map digitToInt) <$> readInputLines
  let lowPointCords = getLowPointsCoords input
  let bMap = fillBasins input (initBmap lowPointCords) ([(l, l) | l <- lowPointCords])
  let basinSizes = getBasinSizes bMap
  let top3 = take 3 (sortBy (flip compare) basinSizes)
  print $ product top3
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]