module Day22.Mod where

import qualified Data.HashMap.Strict as Map
import Data.Ix
import GHC.Arr
import Utils.Mod

data Point = P !Int !Int !Int
  deriving (Read, Show, Ord, Eq)

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

px, py, pz :: Point -> Int
px (P x _ _) = x
py (P _ y _) = y
pz (P _ _ z) = z

-- (low, high)
type Range = (Point, Point)

part1 :: IO ()
part1 = do
  input <- readInputLines
  print "part1"
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]