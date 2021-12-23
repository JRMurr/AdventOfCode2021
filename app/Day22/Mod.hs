module Day22.Mod where

import Data.Char (isDigit)
import qualified Data.HashMap.Strict as Map
-- import Data.Ix
import Data.List.Split (splitOn)
-- import GHC.Arr

import Data.Maybe
import Utils.Mod

data Point = P !Int !Int !Int
  deriving (Read, Show, Ord, Eq)

px, py, pz :: Point -> Int
px (P x _ _) = x
py (P _ y _) = y
pz (P _ _ z) = z

-- https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC.Ix.html#local-6989586621679535302
-- instance Ix Point where
--   unsafeIndex (P l1 l2 l3, P u1 u2 u3) (P i1 i2 i3) =
--     unsafeIndex (l3, u3) i3 + unsafeRangeSize (l3, u3)
--       * ( unsafeIndex (l2, u2) i2 + unsafeRangeSize (l2, u2)
--             * unsafeIndex (l1, u1) i1
--         )

--   inRange (P lox loy loz, P hix hiy hiz) (P x y z) =
--     lox <= x && x <= hix
--       && loy <= y
--       && y <= hiy
--       && loz <= z
--       && z <= hiz

--   range (P lox loy loz, P hix hiy hiz) =
--     [P x y z | x <- [lox .. hix], y <- [loy .. hiy], z <- [loz .. hiz]]

type Range = (Point, Point)

-- lower bound, upper bound, isLit
data Cuboid = C !Range !Bool deriving (Show, Eq)

inRange :: Range -> Point -> Bool
inRange (P lox loy loz, P hix hiy hiz) (P x y z) =
  lox <= x && x <= hix
    && loy <= y
    && y <= hiy
    && loz <= z
    && z <= hiz

area :: Range -> Int
area (lower, upper) = lengthAxis px * lengthAxis py * lengthAxis pz
  where
    -- add 1 since bounds are inclusive
    lengthAxis axisFunc = axisFunc upper - axisFunc lower + 1

-- >>> getIntersection ((P 11 11 11), (P 13 15 13)) ((P 12 12 9), (P 14 14 14))
-- Just (P 12 12 11,P 13 14 13)
-- >>> getIntersection ((P 12 12 9), (P 14 14 14)) ((P 11 11 11), (P 13 15 13))
-- Just (P 12 12 11,P 13 14 13)
-- >>> getIntersection ((P 11 11 11), (P 13 15 13)) ((P 12 14 10), (P 14 14 14))
-- Just (P 12 14 11,P 13 14 13)
-- >>> getIntersection ((P 11 11 11), (P 12 15 12)) ((P 14 14 14), (P 14 14 14))
-- Just (P 12 14 12,P 14 14 14)
-- >>> getIntersection ((P 11 11 11), (P 12 12 12)) ((P 14 14 14), (P 14 14 14))
-- Nothing
-- >>> getIntersection ((P 14 14 14), (P 14 14 14)) ((P 11 11 11), (P 12 12 12))
-- Nothing
getIntersection :: Range -> Range -> Maybe Range
getIntersection r1@(lo1, hi1) r2@(lo2, hi2) = if isValid then Just newRange else Nothing
  where
    getLowerBound axisFunc = axisFunc lo1 `max` axisFunc lo2
    getUpperBound axisFunc = axisFunc hi1 `min` axisFunc hi2
    getBounds axisFunc = (getLowerBound axisFunc, getUpperBound axisFunc)
    -- let (b1, b2) = (getLowerBound axisFunc, getUpperBound axisFunc)
    --  in (b1 `min` b2, b1 `max` b2)
    newRange@(lo, hi) = axisBoundsToRange [getBounds px, getBounds py, getBounds pz]
    isValidAxis f = f lo <= f hi
    isValid = all isValidAxis [px, py, pz]

isLitStr :: String -> Bool
isLitStr "on" = True
isLitStr "off" = False
isLitStr _ = error "invalid lit"

cuboidIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
cuboidIntersection (C r1 on1) (C r2 on2) = newCuboid <$> newRange
  where
    newRange = getIntersection r1 r2
    newCuboid r = C r (not on1)

cuboidArea :: Cuboid -> Int
cuboidArea (C r on) = boolMul * area r
  where
    boolMul = if on then 1 else -1

runSteps :: [Cuboid] -> [Cuboid]
runSteps cubes = foldl reducer [head cubes] (tail cubes)
  where
    reducer acc c@(C _ isOn) = res
      where
        intersections = acc ++ mapMaybe (`cuboidIntersection` c) acc
        res = if isOn then c : intersections else intersections

-- >>> parseAxis "x=-11..13"
-- (-11,13)
parseAxis :: String -> (Int, Int)
parseAxis s = (low, high)
  where
    rangeStr = tail (dropWhile (/= '=') s)
    [low, high] = map read $ splitOn ".." rangeStr

axisBoundsToRange :: [(Int, Int)] -> Range
axisBoundsToRange [(lox, hix), (loy, hiy), (loz, hiz)] = (P lox loy loz, P hix hiy hiz)
axisBoundsToRange _ = error "invalid length"

-- >>> parseRegion "x=10..12,y=10..12,z=10..12"
-- (P 10 10 10,P 12 12 12)
parseRegion :: String -> Range
parseRegion s = axisBoundsToRange (map parseAxis $ splitOn "," s)

parseCuboid :: String -> Cuboid
parseCuboid s = C range (isLitStr litStr)
  where
    [litStr, region] = words s
    range = parseRegion region

getAllCuboids :: IO [Cuboid]
getAllCuboids = map parseCuboid <$> readInputLines

inStartArea :: Cuboid -> Bool
inStartArea (C (lo, hi) _) = all checkBound [px, py, pz]
  where
    checkLo f = f lo >= -50
    checkHi f = f hi <= 50
    checkBound f = checkLo f && checkHi f

part1 :: IO ()
part1 = do
  input <- getAllCuboids
  let valid = filter inStartArea input
  let tmp = runSteps valid
  -- print tmp
  -- print $ map cuboidArea tmp
  print $ sum $ map cuboidArea tmp
  return ()

part2 :: IO ()
part2 = do
  input <- getAllCuboids
  let tmp = runSteps input
  print $ sum $ map cuboidArea tmp
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
