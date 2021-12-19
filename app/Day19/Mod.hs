module Day19.Mod where

import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Mod

type Coord = (Int, Int, Int)

type Scanners = IntMap [Coord]

type Beacons = [Coord]

type RotationMult = (Int, Int, Int)

-- rotation and num swaps to do
type Orientation = (RotationMult, Int)

parseScannerInput :: [String] -> (Int, [Coord])
parseScannerInput (header : coordStr) = (getScannerNum header, map parseCoord coordStr)
  where
    -- i should really figure out regexs...
    getScannerNum :: String -> Int
    getScannerNum s = read numStr
      where
        dropped = dropWhile (not . isDigit) s
        numStr = takeWhile isDigit dropped
    parseCoord :: String -> Coord
    parseCoord s = (x, y, z)
      where
        [x, y, z] = map read $ splitOn "," s
parseScannerInput _ = error "sad"

getScanners :: [String] -> Scanners
getScanners = Map.fromList . map parseScannerInput . splitOn [""]

readScannerMap :: IO Scanners
readScannerMap = getScanners <$> readInputLines

-- Multiply a coord by an element in this to "rotate" it
rotationMults :: [RotationMult]
rotationMults = [(x, y, z) | x <- r, y <- r, z <- r]
  where
    r = [-1, 1]

rotateCoord :: RotationMult -> Coord -> Coord
rotateCoord (xm, ym, zm) (x, y, z) = (x * xm, y * ym, z * zm)

swapNTimes :: Int -> Coord -> Coord
swapNTimes n _ | n > 3 = error ("bad n:" ++ show n)
swapNTimes n c = iterate swapPoints c !! n
  where
    swapPoints (x, y, z) = (z, x, y)

allOrientations :: [Orientation]
allOrientations = cartProd rotationMults [0 .. 2]

getOrientation :: Orientation -> Coord -> Coord
getOrientation (r, n) c = rotateCoord r $ swapNTimes n c

applyOrientation :: Orientation -> [Coord] -> [Coord]
applyOrientation o = map (getOrientation o)

addCoord, subCoord :: Coord -> Coord -> Coord
addCoord (x, y, z) (x', y', z') = (x + x', y + y', z + z')
subCoord (x, y, z) (x', y', z') = (x - x', y - y', z - z')

-- TODO: might be a good idea to pass coords as sets to avoid conversions
-- get all pairs from the list of coords, diff the pair to try to normalize the second set
-- returns the updated second param if theres a match
detectMatch :: [Coord] -> [Coord] -> Maybe [Coord]
detectMatch goodSet b = find isGood allAdjusted
  where
    diffs = map (uncurry subCoord) (cartProd goodSet b)
    getAdjusted diff = map (addCoord diff) b
    allAdjusted = map getAdjusted diffs
    isGood coords = Set.size (getCommon goodSet coords) >= 12

tryMatch :: [Coord] -> [Coord] -> Maybe [Coord]
tryMatch goodSet b = listToMaybe possibleMatches
  where
    bOrientations = map (`applyOrientation` b) allOrientations
    possibleMatches = mapMaybe (detectMatch goodSet) bOrientations

getCommon :: [Coord] -> [Coord] -> Set Coord
getCommon a b = Set.intersection (toSet a) (toSet b)
  where
    toSet x = Set.fromList x

-- TODO: go over all pairs of sensors and try to match up beacons
-- start with sensor 0 as good set

part1 :: IO ()
part1 = do
  input <- readScannerMap
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]