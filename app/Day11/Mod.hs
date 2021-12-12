module Day11.Mod where

import Data.Char (digitToInt, intToDigit)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Utils.Coords
import Utils.Mod

type EnergyLevels = Map Coord Int

type FlashedCoords = Set Coord

getInputMap :: IO EnergyLevels
getInputMap = M.fromList . coordLinesInt . map (map digitToInt) <$> readInputLines

increasePos :: (Ord k, Num a) => k -> Map k a -> Map k a
increasePos = M.adjust (1 +)

increaseNeighbors :: (EnergyLevels, FlashedCoords) -> Coord -> (EnergyLevels, FlashedCoords)
increaseNeighbors (e, flashedSet) c = foldr handleCoord (e, flashedSet) neighborsCoords
  where
    neighborsCoords = neighbors c
    handleCoord pos info = handleFlashes info pos

handleFlashes :: (EnergyLevels, FlashedCoords) -> Coord -> (EnergyLevels, FlashedCoords)
-- handleFlashes e coord val | val > 9 = increaseNeighbors e coord
-- handleFlashes e _ _ = e
handleFlashes res@(e, _) coord | not $ coord `M.member` e = res
handleFlashes res@(e, flashed) coord | coord `S.member` flashed = res
handleFlashes (e, flashed) coord = if newVal > 9 then increaseNeighbors (updatedPos, updatedSet) coord else (updatedPos, flashed)
  where
    updatedPos = increasePos coord e
    newVal = (M.!) updatedPos coord
    updatedSet = S.insert coord flashed

-- return map after this step and num flashes on this step
runStep :: EnergyLevels -> (EnergyLevels, FlashedCoords)
runStep e = (resetELevels, flashedSet)
  where
    (updatedMap, flashedSet) = M.foldrWithKey (\coord _ eMap -> handleFlashes eMap coord) (e, S.empty) e
    resetELevels = M.map (\val -> if val > 9 then 0 else val) updatedMap

runNSteps :: EnergyLevels -> Int -> (Int, EnergyLevels)
runNSteps e 0 = (0, e)
runNSteps e n = (getNumFlashes flashSet + recNumFlashes, finishedE)
  where
    getNumFlashes flashSet = S.size flashSet
    (updatedE, flashSet) = runStep e
    (recNumFlashes, finishedE) = runNSteps updatedE (n -1)

displayMap :: EnergyLevels -> String
displayMap e = drawCoords (M.map intToDigit e)

part1 :: IO ()
part1 = do
  input <- getInputMap
  let (numFlashes, res) = runNSteps input 100
  putStr $ displayMap res
  print numFlashes
  return ()

getStepAllFlash :: EnergyLevels -> Int -> Int
getStepAllFlash e count = if S.size flashSet == M.size updatedMap then count else getStepAllFlash updatedMap (count + 1)
  where
    (updatedMap, flashSet) = runStep e

part2 :: IO ()
part2 = do
  input <- getInputMap
  print $ getStepAllFlash input 1
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]