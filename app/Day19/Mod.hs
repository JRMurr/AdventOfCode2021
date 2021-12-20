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

data Rotation = RotateX | RotateY | RotateZ
  deriving (Eq, Show)

-- All distinct rotations
allRotations :: [[Rotation]]
allRotations = concatMap rz [[], [RotateX], [RotateX, RotateX], [RotateX, RotateX, RotateX], [RotateY], [RotateY, RotateY, RotateY]]
  where
    rz v = [v, RotateZ : v, RotateZ : RotateZ : v, RotateZ : RotateZ : RotateZ : v]

rotate :: Coord -> [Rotation] -> Coord
rotate = foldl rotate'
  where
    rotate' (x, y, z) RotateX = (x, z, - y)
    rotate' (x, y, z) RotateY = (- z, y, x)
    rotate' (x, y, z) RotateZ = (y, - x, z)

applyRotation :: ScannerSet -> [Rotation] -> ScannerSet
applyRotation coords r = Set.map (`rotate` r) coords

addCoord, subCoord :: Coord -> Coord -> Coord
addCoord (x, y, z) (x', y', z') = (x + x', y + y', z + z')
subCoord (x, y, z) (x', y', z') = (x - x', y - y', z - z')

type AdjustedScanner = (ScannerSet, Coord) -- beacons cords with the diff used to make the map

getAllPossibleOrientations :: ScannerSet -> [ScannerSet]
getAllPossibleOrientations toCheck = map (applyRotation toCheck) allRotations

tryMatch :: AdjustedScanner -> ScannerSet -> Maybe AdjustedScanner
tryMatch (goodSet, _) toCheck = listToMaybe possibleMatches
  where
    possibleMatches = mapMaybe detectMatch (getAllPossibleOrientations toCheck)
    detectMatch :: ScannerSet -> Maybe AdjustedScanner
    detectMatch orientedCheck = find isGood allAdjusted
      where
        diffs = Set.map (uncurry subCoord) (Set.cartesianProduct goodSet orientedCheck)
        getAdjusted :: Coord -> AdjustedScanner
        getAdjusted diff = (Set.map (addCoord diff) orientedCheck, diff)
        allAdjusted = Set.map getAdjusted diffs -- traceShow ("goodSet len:", Set.size goodSet, "toCheck size:", Set.size orientedCheck, "diffs len:", Set.size diffs) $
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
       in traceShow ("remaining: ", length newToCheck) $ fullMatch' newAdjusted newToCheck

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

manhattan :: Coord -> Coord -> Int
manhattan (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

getMaxManhattan :: [Coord] -> Int
getMaxManhattan coords = maximum [manhattan x y | (x, y) <- cartProd coords coords]

part2 :: IO ()
part2 = do
  input <- readScannerMap
  let (_, diffs) = fullMatch input
  print $ getMaxManhattan diffs
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]