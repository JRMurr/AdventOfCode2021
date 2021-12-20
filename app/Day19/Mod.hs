module Day19.Mod where

import Data.Bifunctor
import Data.Char (isDigit)
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Utils.Mod

type Coord = (Int, Int, Int)

type ScannerSet = Set Coord

type Scanners = IntMap ScannerSet

type RotationMult = (Int, Int, Int)

-- rotation and num swaps to do
type Orientation = (RotationMult, Int)

parseScannerInput :: [String] -> (Int, ScannerSet)
parseScannerInput (header : coordStr) = (getScannerNum header, Set.fromList (map parseCoord coordStr))
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
    r = [1, -1]

rotateCoord :: RotationMult -> Coord -> Coord
rotateCoord (xm, ym, zm) (x, y, z) = (x * xm, y * ym, z * zm)

swapNTimes :: Int -> Coord -> Coord
-- swapNTimes n _ | n > 3 = error ("bad n:" ++ show n)
swapNTimes n c = iterate swapPoints c !! n
  where
    swapPoints (x, y, z) = (z, x, y)

allOrientations :: [Orientation]
allOrientations = cartProd rotationMults [0 .. 2]

getOrientation :: Orientation -> Coord -> Coord
getOrientation (r, n) c = swapNTimes n $ rotateCoord r c

applyOrientation :: Orientation -> ScannerSet -> ScannerSet
applyOrientation o = Set.map (getOrientation o)

addCoord, subCoord :: Coord -> Coord -> Coord
addCoord (x, y, z) (x', y', z') = (x + x', y + y', z + z')
subCoord (x, y, z) (x', y', z') = (x - x', y - y', z - z')

type AdjustedScanner = (ScannerSet, Coord) -- beacons cords with the diff used to make the map

-- get all pairs from the list of coords, diff the pair to try to normalize the second set
-- returns the updated second param and diff if theres a match
-- detectMatch :: ScannerSet -> ScannerSet -> Maybe AdjustedScanner
-- detectMatch goodSet toCheck = find isGood allAdjusted
--   where
--     diffs = Set.map (uncurry subCoord) (Set.cartesianProduct goodSet toCheck)
--     getAdjusted diff = (Set.map (addCoord diff) toCheck, diff)
--     allAdjusted = Set.map getAdjusted diffs
--     intersectSize coords = Set.size (Set.intersection goodSet coords)
--     isGood (coords, _) = intersectSize coords >= 12

tryMatch :: AdjustedScanner -> ScannerSet -> Maybe AdjustedScanner
tryMatch (goodSet, _) toCheck = listToMaybe possibleMatches
  where
    bOrientations = map (`applyOrientation` toCheck) allOrientations
    possibleMatches = mapMaybe detectMatch bOrientations
    detectMatch :: ScannerSet -> Maybe AdjustedScanner
    detectMatch orientedCheck = find isGood allAdjusted
      where
        diffs = Set.map (uncurry subCoord) (Set.cartesianProduct goodSet orientedCheck)
        getAdjusted diff = (Set.map (addCoord diff) orientedCheck, diff)
        allAdjusted = Set.map getAdjusted diffs
        intersectSize coords = Set.size (Set.intersection goodSet coords)
        isGood (coords, _) = intersectSize coords >= 12

-- Given set list of good scaners and a new try to match up scanners
-- if match add adjust curr to list of good
stepMatch :: [AdjustedScanner] -> ScannerSet -> Maybe AdjustedScanner
stepMatch adjustedScanners curr = listToMaybe $ mapMaybe (`tryMatch` curr) adjustedScanners

-- stepMatch adjustedScanners curr = ((`tryMatch` curr) . fst) (joinAdjusted adjustedScanners)

findNextScanner :: [AdjustedScanner] -> [ScannerSet] -> ([AdjustedScanner], [ScannerSet])
findNextScanner adjustedScanners toCheckLst = head $ mapMaybe tryStep picked
  where
    picked = pickOne toCheckLst
    getOutput :: [ScannerSet] -> AdjustedScanner -> ([AdjustedScanner], [ScannerSet])
    getOutput rest adjusted = (adjusted : adjustedScanners, rest)
    tryStep :: (ScannerSet, [ScannerSet]) -> Maybe ([AdjustedScanner], [ScannerSet])
    tryStep (toTry, rest) = getOutput rest <$> stepMatch adjustedScanners toTry

-- fullMatch :: Scanners -> (ScannerSet, [Coord])
-- fullMatch s = foldl' (\(accSet, accDiff) (set, diff) -> (Set.union accSet set, diff : accDiff)) (Set.empty, []) allSets
--   where
--     initGood = [((Map.!) s 0, (0, 0, 0))]
--     toCheckLst = Map.elems (Map.delete 0 s)
--     allSets = foldl' (stepMatch) initGood toCheckLst

joinAdjusted :: [AdjustedScanner] -> (ScannerSet, [Coord])
joinAdjusted = foldl' (\(accSet, accDiff) (set, diff) -> (Set.union accSet set, diff : accDiff)) (Set.empty, [])

fullMatch :: Scanners -> (ScannerSet, [Coord])
fullMatch s = joinAdjusted finalAdjusted
  where
    initAdjusted = [((Map.!) s 0, (0, 0, 0))]
    toCheckLst = Map.elems (Map.delete 0 s)
    finalAdjusted = fullMatch' initAdjusted toCheckLst
    fullMatch' :: [AdjustedScanner] -> [ScannerSet] -> [AdjustedScanner]
    fullMatch' x [] = x
    fullMatch' adjusted toCheck =
      let (newAdjusted, newToCheck) = findNextScanner adjusted toCheck
       in fullMatch' newAdjusted newToCheck

showCoord :: Coord -> String
showCoord (x, y, z) = show x ++ "," ++ show y ++ "," ++ show z

part1 :: IO ()
part1 = do
  input <- readScannerMap
  let (allBeacons, diffs) = fullMatch input
  let sorted = sort $ Set.elems allBeacons
  -- mapM_ (putStrLn . showCoord) sorted
  print "--------"
  print $ Set.size allBeacons
  print $ diffs
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]