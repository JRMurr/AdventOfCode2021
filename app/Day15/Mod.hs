module Day15.Mod where

import Data.Bifunctor (bimap)
import Data.Char (digitToInt, intToDigit)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable (Hashable)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.PQueue.Prio.Min as PQ
import Debug.Trace
import Utils.Coords
import Utils.Mod

-- I totally made this
-- A* search: Finds the shortest path from a start node to a goal node using a heuristic function.
astarSearch ::
  (Eq a, Hashable a) =>
  a -> -- startNode: the node to start the search from
  (a -> Bool) -> -- isGoalNode: a function to test if a node is the goal node
  (a -> [(a, Int)]) -> -- nextNodeFn: a function which calculates the next nodes for a current node
  -- along with the costs of moving from the current node to the next nodes
  (a -> Int) -> -- heuristic: a function which calculates the (approximate) cost of moving
  -- from a node to the nearest goal node
  Maybe (Int, [a]) -- result: Nothing is no path is found else
  -- Just (path cost, path as a list of nodes)
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar
    (PQ.singleton (heuristic startNode) (startNode, 0))
    Set.empty
    (Map.singleton startNode 0)
    Map.empty
  where
    -- pq: open set, seen: closed set, tracks: tracks of states
    astar pq seen gscore tracks
      -- If open set is empty then search has failed. Return Nothing
      | PQ.null pq = Nothing
      -- If goal node reached then construct the path from the tracks and node
      | isGoalNode node = Just (gcost, findPath tracks node)
      -- If node has already been seen then discard it and continue
      | Set.member node seen = astar pq' seen gscore tracks
      -- Else expand the node and continue
      | otherwise = astar pq'' seen' gscore' tracks'
      where
        -- Find the node with min f-cost
        (node, gcost) = snd . PQ.findMin $ pq

        -- Delete the node from open set
        pq' = PQ.deleteMin pq

        -- Add the node to the closed set
        seen' = Set.insert node seen

        -- Find the successors (with their g and h costs) of the node
        -- which have not been seen yet
        successors =
          filter
            ( \(s, g, _) ->
                not (Set.member s seen')
                  && ( not (s `Map.member` gscore)
                         || g < (fromJust . Map.lookup s $ gscore)
                     )
            )
            $ successorsAndCosts node gcost

        -- Insert the successors in the open set
        pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors

        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors

        -- Insert the tracks of the successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors

    -- Finds the successors of a given node and their costs
    successorsAndCosts node gcost =
      map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node

    -- Constructs the path from the tracks and last node
    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
        else [node]

type Positions = Map Coord Int

getPositions :: IO Positions
getPositions = M.fromList . coordLinesInt . map (map digitToInt) <$> readInputLines

getNextNode :: Positions -> Coord -> [(Coord, Int)]
getNextNode posMap curr = [(c, (M.!) posMap c) | c <- nextCoords, M.member c posMap]
  where
    nextCoords = neighborsCardinal curr

runAStar :: Positions -> Int
runAStar posMap = cost
  where
    Just (start, end) = boundingBox (M.keys posMap)
    Just (cost, _) = astarSearch start (== end) (getNextNode posMap) (const 0)

part1 :: IO ()
part1 = do
  input <- getPositions
  print $ runAStar input
  return ()

getNewVal :: Int -> Int -> Int
getNewVal startValue incrAmount = if added > 9 then added `mod` 9 else added
  where
    added = startValue + incrAmount

getNewCoords :: (Int, Int) -> Coord -> Int -> [(Coord, Int)]
getNewCoords (width, height) startCoord startVal = map (bimap (addCoord startCoord) (getNewVal startVal)) joinOffsets
  where
    getOffsets :: Int -> [(Int, Int)]
    getOffsets stepSize = map (\idx -> (stepSize * idx, idx)) [0 .. 4]
    allOffsets = cartProd (getOffsets width) (getOffsets height)
    joinOffsets = map (\((xOffset, xIncr), (yOffset, yIncr)) -> (C xOffset yOffset, xIncr + yIncr)) allOffsets

getFullMap :: Positions -> Positions
getFullMap posMap = M.fromList newCoords
  where
    Just (_, C width height) = boundingBox (M.keys posMap)
    posList = M.assocs posMap
    newCoords = concatMap (uncurry (getNewCoords (width + 1, height + 1))) posList

displayMap e = drawCoords (M.map intToDigit e)

part2 :: IO ()
part2 = do
  input <- getPositions
  let newMap = getFullMap input
  -- print $ boundingBox (M.keys newMap)
  -- putStr $ displayMap newMap
  print $ runAStar (getFullMap input)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]