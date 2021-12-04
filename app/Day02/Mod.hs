module Day02.Mod where

import Data.List
import Data.List.Split
import Utils.Coords
import Utils.Mod

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

splitOnSpacePair :: String -> (String, Int)
splitOnSpacePair str = let (x, y) = tuplify2 $ splitOn " " str in (x, read y)

readCord :: (String, Int) -> Coord
readCord ("forward", x) = C x 0
readCord ("up", x) = C 0 (- x) -- depth is positive so up is neg
readCord ("down", x) = C 0 x

part1 :: IO ()
part1 = do
  input <- readInputLines
  let dirs = map (readCord . splitOnSpacePair) input
  let endCord = foldl addCoord origin dirs
  print endCord
  return ()

-- horizontal, aim, depth
type CordAim = (Int, Int, Int)

addCordAim :: CordAim -> Coord -> CordAim
-- forward
addCordAim (x1, aim, d1) (C x2 0) = (x1 + x2, aim, d1 + (x2 * aim))
-- down/up change aim
addCordAim (x1, aim, d1) (C 0 y) = (x1, aim + y, d1)

getAnswer :: CordAim -> Int
getAnswer (x, aim, d) = x * d

part2 :: IO ()
part2 = do
  input <- readInputLines
  let dirs = map (readCord . splitOnSpacePair) input
  let endCord = foldl addCordAim (0, 0, 0) dirs
  print $ getAnswer endCord
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]