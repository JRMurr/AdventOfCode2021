module Day22.Mod where

import Data.Char (isDigit)
import qualified Data.HashMap.Strict as Map
-- import Data.Ix
import Data.List.Split (splitOn)
-- import GHC.Arr
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
data Cuboid = C !Range !Bool deriving (Show)

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
    getBounds axisFunc =
      let (b1, b2) = (getLowerBound axisFunc, getUpperBound axisFunc)
       in (b1 `min` b2, b1 `max` b2)
    newRange@(lo, hi) = axisBoundsToRange [getBounds px, getBounds py, getBounds pz]
    isValid = not ((hi1 == lo || hi2 == lo) && (lo1 == hi || lo2 == hi))

isLitStr :: String -> Bool
isLitStr "on" = True
isLitStr "off" = False
isLitStr _ = error "invalid lit"

-- >>> parseAxis "x=11..13"
-- (11,13)
parseAxis :: String -> (Int, Int)
parseAxis s = (low, high)
  where
    rangeStr = dropWhile (not . isDigit) s
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

part1 :: IO ()
part1 = do
  input <- getAllCuboids
  print input
  return ()

part2 :: IO ()
part2 = do
  input <- getAllCuboids
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
