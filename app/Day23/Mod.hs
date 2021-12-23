{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Day23.Mod where

import Data.Hashable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace
import GHC.Generics (Generic)
import Utils.AStar
import Utils.Coords
import Utils.Mod

data Amphipod = Amber | Bronze | Copper | Desert deriving (Show, Eq, Ord, Generic)

instance Hashable Amphipod

data Tile = Open | Wall | Amp Amphipod deriving (Show, Eq, Ord, Generic)

instance Hashable Tile

type Burrow = Map Coord Tile

toTile :: Char -> Maybe Tile
toTile '#' = Just Wall
toTile '.' = Just Open
toTile 'A' = Just (Amp Amber)
toTile 'B' = Just (Amp Bronze)
toTile 'C' = Just (Amp Copper)
toTile 'D' = Just (Amp Desert)
toTile _ = Nothing

tileToChar :: Tile -> Char
tileToChar Wall = '#'
tileToChar Open = '.'
tileToChar (Amp Amber) = 'A'
tileToChar (Amp Bronze) = 'B'
tileToChar (Amp Copper) = 'C'
tileToChar (Amp Desert) = 'D'

coordCharToTile :: (Coord, Char) -> Maybe (Coord, Tile)
coordCharToTile (c, char) = case toTile char of
  Just t -> Just (c, t)
  Nothing -> Nothing

toBurrow :: [String] -> Burrow
toBurrow = M.fromList . mapMaybe coordCharToTile . coordLines

readBurrow :: IO Burrow
readBurrow = toBurrow <$> readInputLines

readGoal :: IO Burrow
readGoal = toBurrow . lines <$> readDayFile 23 "goal"

energy :: Amphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

roomCol :: Amphipod -> Int
roomCol Amber = 3
roomCol Bronze = 5
roomCol Copper = 7
roomCol Desert = 9

isGoalRoom :: Amphipod -> Coord -> Bool
isGoalRoom amp c = S.member c allowedRooms
  where
    allowedRooms = S.filter (\(C _ col) -> col == roomCol amp) roomCoords

getGoalRooms :: Amphipod -> [Coord]
getGoalRooms amp = S.elems $ S.filter (\(C _ col) -> col == roomCol amp) roomCoords

-- roomCoords :: Set Coord
-- roomCoords = S.fromList [C row col | row <- [2, 3], col <- [3, 5, 7, 9]]

roomCoords :: Set Coord
roomCoords = S.fromList [C row col | row <- [2, 3, 4, 5], col <- [3, 5, 7, 9]]

hallCoords :: Set Coord
hallCoords = S.fromList [C 1 col | col <- [1 .. 12]]

allPossibleOpen :: [Coord]
allPossibleOpen = S.elems $ S.union roomCoords hallCoords

-- validStoppingHallCoords :: [Coord]
-- validStoppingHallCoords = filter (not . isAboveRoom) (S.elems hallCoords)

isAmpTile :: Tile -> Bool
isAmpTile (Amp _) = True
isAmpTile _ = False

-- | Applies `f` on the tile at the coord specified, returns `False` if the coord is not in the map
checkTileInMap :: (Tile -> Bool) -> Burrow -> Coord -> Bool
checkTileInMap _ bMap c | not (M.member c bMap) = False
checkTileInMap f bMap c = f ((M.!) bMap c)

checkIsAmp :: Burrow -> Coord -> Bool
checkIsAmp = checkTileInMap isAmpTile

checkIsOpenOrAmpType :: Amphipod -> Burrow -> Coord -> Bool
checkIsOpenOrAmpType a = checkTileInMap (\t -> t == Amp a || t == Open)

isRoomCoord, isHallCoord :: Coord -> Bool
isRoomCoord c = S.member c roomCoords
isHallCoord c = S.member c hallCoords

isAboveRoom :: Coord -> Bool
isAboveRoom c = isHallCoord c && isRoomCoord (below c)

-- Can not stop directly above a room
-- Will only move into room if its empty or is its goal room
-- Once in hallway is locked in place until it can go into a room

calcEnergy :: Amphipod -> Coord -> Coord -> Int
calcEnergy a c1 c2 = energy a * manhattan c1 c2

isOpenTile :: Burrow -> Coord -> Bool
isOpenTile = checkTileInMap (== Open)

type FollowDirFn = Coord -> Coord

takeWhileOpen :: Burrow -> Coord -> FollowDirFn -> [Coord]
takeWhileOpen bMap c f = takeWhile (isOpenTile bMap) (tail $ iterate f c)

getValidMovesFromRoom :: Burrow -> Coord -> [(Coord, Int)]
getValidMovesFromRoom _ c | not (isRoomCoord c) = []
getValidMovesFromRoom bMap c | not (checkIsAmp bMap c) = []
getValidMovesFromRoom bMap c | checkIsAmp bMap (above c) = []
getValidMovesFromRoom bMap c =
  if isGoalRoom currAmp c && checkTileInMap (\t -> t == Amp currAmp || t == Wall) bMap (below c)
    then []
    else energies
  where
    (Amp currAmp) = (M.!) bMap c
    hallCord = until (`S.member` hallCoords) above c
    hallEnergy = calcEnergy currAmp c hallCord
    validDir f = filter (not . isAboveRoom) $ takeWhileOpen bMap hallCord f
    validHalls = concatMap validDir [left, right]
    energies = map (\newc -> (newc, hallEnergy + calcEnergy currAmp hallCord newc)) validHalls

getCostsRelative :: Amphipod -> Coord -> Coord -> (Coord, Int)
getCostsRelative a relC newC = (newC, calcEnergy a relC newC)

getValidMovesFromHall :: Burrow -> Coord -> [(Coord, Int)]
getValidMovesFromHall _ c | not (isHallCoord c) = []
getValidMovesFromHall bMap c | checkIsAmp bMap c = if roomValid then roomEnergies else []
  where
    (Amp currAmp) = (M.!) bMap c
    openHalls = concatMap (takeWhileOpen bMap c) [left, right]
    aboveRooms = filter isAboveRoom openHalls
    roomValid = all (checkIsOpenOrAmpType currAmp bMap) (getGoalRooms currAmp)
    goalRoomHall = find (isGoalRoom currAmp . below) aboveRooms
    getOpenRooms hallC = filter (not . isAboveRoom) $ takeWhileOpen bMap hallC below
    openRooms = concat . maybeToList $ getOpenRooms <$> goalRoomHall
    safeGoalRoomHall = fromJust goalRoomHall
    safeGoalRoomHallE = calcEnergy currAmp c safeGoalRoomHall
    roomEnergies = map (\newC -> (newC, safeGoalRoomHallE + calcEnergy currAmp safeGoalRoomHall newC)) openRooms
getValidMovesFromHall _ _ = []

applyMove :: Burrow -> (Coord, (Coord, Int)) -> (Burrow, Int)
applyMove bMap (start, (end, cost)) = (updatedEnd, cost)
  where
    startTile = (M.!) bMap start
    changedStart = M.insert start Open bMap
    updatedEnd = M.insert end startTile changedStart

getValidMoves :: Burrow -> [(Burrow, Int)]
getValidMoves bMap = map (applyMove bMap) allMoves
  where
    runGet :: (Burrow -> Coord -> [(Coord, Int)]) -> [(Coord, (Coord, Int))]
    runGet f = concatMap (\c -> let res = f bMap c in map (c,) res) allPossibleOpen
    allMoves = concatMap runGet [getValidMovesFromRoom, getValidMovesFromHall]

callAStar :: Burrow -> Int
callAStar start = case res of
  Just (i, _) -> i
  _ -> error "no valid path"
  where
    -- isGoal x = x == goal
    res = astarSearch start isGoal getValidMoves (const 0)

drawBurrow :: Burrow -> String
drawBurrow = drawCoordsGen tileToChar Open

debugMove :: Burrow -> Coord -> Coord -> Burrow
debugMove bMap start end = let (res, _) = applyMove bMap (start, (end, 0)) in res

isGoal :: Burrow -> Bool
isGoal bMap = all isGoalAmp ampCoords
  where
    ampCoords = M.toList $ M.filter isAmpTile bMap
    isGoalAmp (coord, Amp a) = isGoalRoom a coord

debug :: Burrow -> IO ()
debug input = do
  let (start, end) = (C 2 7, C 1 4)
  let tmp = debugMove input start end
  putStrLn $ drawBurrow tmp
  -- print $ getValidMovesFromHall tmp (C 2 5)
  -- print $ getValidMovesFromRoom tmp (C 2 5)
  let tmp2 = debugMove tmp (C 2 5) (C 1 6)
  putStrLn $ drawBurrow tmp2
  -- print $ getValidMovesFromHall tmp2 (C 1 6)
  let tmp3 = debugMove tmp2 (C 1 6) (C 2 7)
  putStrLn $ drawBurrow tmp3

  -- print $ M.filter isAmpTile tmp3
  -- print $ getValidMovesFromRoom tmp3 (C 3 5)

  -- print $ getValidMovesFromHall tmp3 end
  let tmp4 = debugMove tmp3 (C 3 5) (C 1 6)
  putStrLn $ drawBurrow tmp4

  -- print $ getValidMovesFromHall tmp4 end
  let tmp5 = debugMove tmp4 end (C 3 5)
  putStrLn $ drawBurrow tmp5
  -- print $ getValidMovesFromRoom tmp5 (C 2 3)

  let tmp6 = debugMove tmp5 (C 2 3) (C 2 5)
  putStrLn $ drawBurrow tmp6

  print $ M.filter isAmpTile tmp6
  print $ getValidMovesFromRoom tmp6 (C 2 9)

  let tmp7 = debugMove tmp6 (C 2 9) (C 1 8)
  putStrLn $ drawBurrow tmp7

  print $ getValidMovesFromRoom tmp7 (C 3 9)
  return ()

part1 :: IO ()
part1 = do
  input <- readBurrow
  goal <- readGoal
  -- test <- debug input
  -- print $ M.filter isAmpTile goal
  -- print $ getValidMovesFromRoom input (C 2 7)
  -- putStrLn $ drawBurrow input
  -- let (start, end) = (C 2 7, C 1 4)
  -- let tmp = debugMove input start end
  -- print $ getValidMovesFromHall tmp (C 2 5)
  -- print $ getValidMovesFromRoom tmp (C 2 5)
  -- let tmp2 = debugMove tmp (C 2 5) (C 1 6)
  -- print $ getValidMovesFromHall tmp (C 2 5)
  -- print $ takeWhileOpen tmp (C 1 5) below
  -- print $ M.filter isAmpTile tmp
  -- print $ (M.!) tmp (below (C 1 5))

  print $ callAStar input
  return ()

-- breaking part 1 to do part 2

readPart2 :: IO Burrow
readPart2 = toBurrow . lines <$> readDayFile 23 "in.p2"

part2 :: IO ()
part2 = do
  input <- readPart2
  print $ callAStar input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]