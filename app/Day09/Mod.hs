module Day09.Mod where

import Data.Char (digitToInt)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
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

-- | get all points around coord less until a point has a value of 9
-- expandPoint :: Tubes -> Basin -> Coord -> Basin
-- expandPoint tubes currBasin c
--   | c `elem` currBasin = []
--   | isNothing currVal = []
--   | currVal == Just 9 = []
--   | otherwise = nub $ c : concatMap (expandPoint tubes newBasin) (neighborsCardinal c)
--   where
--     currVal = Map.lookup c tubes
--     newBasin :: Basin
--     newBasin = c : currBasin

-- value is the lowpoint for the coord in the key
type LowPointMap = Map Coord Coord

findLowPointForCoord :: Tubes -> LowPointMap -> Coord -> LowPointMap
findLowPointForCoord tubes lowMap c
  | Map.member c lowMap = lowMap
  | isJust lowPointFromNeighbor = Map.insert c (fromJust lowPointFromNeighbor) lowMap
  | otherwise = Map.insert c getLowFromRecCall lowPointRec
  where
    neighborCoords = neighborsCardinal c
    lowPointFromNeighbor = listToMaybe $ mapMaybe (`Map.lookup` lowMap) neighborCoords
    lowPointRec = foldr (\coord m -> Map.union m (findLowPointForCoord tubes lowMap coord)) lowMap neighborCoords
    getLowFromRecCall = head $ mapMaybe (`Map.lookup` lowPointRec) neighborCoords

-- getBasins :: Tubes -> [Basin]
-- getBasins tubes = parMap rpar (expandPoint tubes []) lowPointCoords
--   where
--     lowPointCoords = getLowPointsCoords tubes
--     removeNines = removeNine tubes

getLowPointMap :: Tubes -> LowPointMap
getLowPointMap tubes = foldr (\c m -> Map.union m (findLowPointForCoord tubes m c)) lowPointMapStart (Map.keys removeNines)
  where
    lowPointCoords = getLowPointsCoords tubes
    removeNines = removeNine tubes
    lowPointMapStart = Map.fromList [(c, c) | c <- lowPointCoords]

removeNine :: Tubes -> Tubes
removeNine = Map.filter (9 /=)

getBasinSizes :: LowPointMap -> [Int]
getBasinSizes lowMap = map length $ group $ sort (Map.elems lowMap)

part2 :: IO ()
part2 = do
  input <- Map.fromList . coordLinesInt . map (map digitToInt) <$> readInputLines
  let lowMap = getLowPointMap input
  let basinSizes = getBasinSizes lowMap
  let top3 = take 3 (sortBy (flip compare) basinSizes)
  -- let basins = getBasins input
  -- let lengths = map length basins
  -- let top3 = take 3 (sortBy (flip compare) lengths)
  print $ product top3
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]