module Day06.Mod where

import Control.Parallel
import Control.Parallel.Strategies
import Data.Function (fix)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import qualified Data.Vector as V
import Debug.Trace
import Utils.Mod

type Timers = [Int]

parseInput :: String -> Timers
parseInput x = map read (splitOn "," x) :: Timers

-- | For a fish timer input, retruns a tuple whose first elem is the updated fish count, the second is the optional new fish
handleFish :: Int -> (Int, Maybe Int)
handleFish 0 = (6, Just 8)
handleFish n = (n -1, Nothing)

updateList :: Int -> Timers -> Timers
updateList curr newLst = case handleFish curr of
  (n, Just new) -> n : new : newLst
  (n, Nothing) -> n : newLst

simulateDay :: Timers -> Timers
simulateDay = foldr updateList []

runDays :: Timers -> Int -> Timers
runDays lst n = last $ take (n + 1) $ iterate simulateDay lst

part1 :: IO ()
part1 = do
  input <- parseInput . head <$> readInputLines
  print $ length $ runDays input 80
  return ()

nextDay :: IntMap Int -> IntMap Int
nextDay = IntMap.foldrWithKey updateMap IntMap.empty

-- The map key is a fish with an internal counter set to that key value, the value for the entry is how many fish are at that count
updateMap :: Int -> Int -> IntMap Int -> IntMap Int
updateMap 0 v = IntMap.insertWith (+) 6 v . IntMap.insert 8 v
updateMap i v = IntMap.insert (i - 1) v

timersToMap :: Timers -> IntMap Int
timersToMap timers = IntMap.fromListWith (+) [(t, 1) | t <- timers]

solve :: IntMap Int -> Int -> Int
solve lst n = sum . IntMap.elems $ last $ take (n + 1) $ iterate nextDay lst

part2 :: IO ()
part2 = do
  input <- parseInput . head <$> readInputLines
  print $ solve (timersToMap input) 256
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]