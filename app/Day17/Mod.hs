module Day17.Mod where

import Data.List.Split
import Data.Maybe
import Debug.Trace
import Utils.Coords
import Utils.Mod

data Target = Target {topLeft :: Coord, bottomRight :: Coord} deriving (Show)

-- Get X or Y bound from input string
parseBound :: String -> (Int, Int)
parseBound (_ : '=' : str) = (read lower, read upper)
  where
    [lower, upper] = splitOn ".." str
parseBound _ = error "sad bound"

parseTarget :: String -> Target
parseTarget str = Target {topLeft = C yUpper xLower, bottomRight = C yLower xUpper}
  where
    [_, _, xStr, yStr] = words str
    (xLower, xUpper) = parseBound (init xStr) -- need init to remove the comma
    (yLower, yUpper) = parseBound yStr

getTarget :: IO Target
getTarget = parseTarget <$> getRawInput

isPastTarget :: Target -> Coord -> Bool
isPastTarget Target {bottomRight = (C ty tx)} (C y x) = y < ty || x > tx

isInTarget :: Target -> Coord -> Bool
isInTarget Target {topLeft = (C ty tx), bottomRight = (C by bx)} (C y x) = checkLeft && checkRight
  where
    checkLeft = y <= ty && x >= tx
    checkRight = y >= by && x <= bx

data Probe = Probe {pos :: Coord, vel :: Coord} deriving (Show)

newXVel :: Int -> Int
newXVel 0 = 0
newXVel x | x > 0 = x -1
newXVel x = x + 1

stepProbe :: Probe -> Probe
stepProbe Probe {pos = p, vel = v@(C vy vx)} = Probe {pos = newPos, vel = C (vy -1) (newXVel vx)}
  where
    newPos = addCoord p v

initProbe :: Coord -> Probe
initProbe vel = Probe {pos = origin, vel = vel}

-- Step probe until it reaches the target (just) or is past it (nothing), returning a list of all of its Coords along the way
runProbeUntilTarget :: Target -> Probe -> Maybe [Coord]
runProbeUntilTarget t p | isInTarget t (pos p) = Just [pos p]
runProbeUntilTarget t p | isPastTarget t (pos p) = Nothing
runProbeUntilTarget t p = fmap (pos p :) recCall
  where
    stepped = stepProbe p
    recCall = runProbeUntilTarget t stepped

getMaxY :: Maybe [Coord] -> Int
getMaxY Nothing = 0
getMaxY (Just lst) = maximum $ map coordRow lst

getValidStartX :: Target -> [Int]
getValidStartX Target {bottomRight = C _ x} = [1 .. x]

findBestStartVel :: Target -> Int
findBestStartVel t@Target {bottomRight = C _ x} = maximum $ map runWithVel vels
  where
    xRange = getValidStartX t
    yRange = [0 .. x] -- needs to fall within max x steps
    vels = [C y x | (x, y) <- cartProd xRange yRange]
    runWithVel c = getMaxY $ runProbeUntilTarget t (initProbe c)

part1 :: IO ()
part1 = do
  target <- getTarget
  print $ findBestStartVel target
  return ()

findAllValidStarts :: Target -> Int
findAllValidStarts t@Target {bottomRight = C _ x} = length $ mapMaybe runWithVel vels
  where
    xRange = getValidStartX t
    yRange = [- x .. x] -- needs to fall within max x steps
    vels = [C y x | (x, y) <- cartProd xRange yRange]
    runWithVel c = runProbeUntilTarget t (initProbe c)

part2 :: IO ()
part2 = do
  target <- getTarget
  print $ findAllValidStarts target
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]