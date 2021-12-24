module Main where

-- need to update other-modules to add sub modules
import qualified Day01.Mod as Day01 (dispatch)
import qualified Day02.Mod as Day02 (dispatch)
import qualified Day03.Mod as Day03 (dispatch)
import qualified Day04.Mod as Day04 (dispatch)
import qualified Day05.Mod as Day05 (dispatch)
import qualified Day06.Mod as Day06 (dispatch)
import qualified Day07.Mod as Day07 (dispatch)
import qualified Day08.Mod as Day08 (dispatch)
import qualified Day09.Mod as Day09 (dispatch)
import qualified Day10.Mod as Day10 (dispatch)
import qualified Day11.Mod as Day11 (dispatch)
import qualified Day12.Mod as Day12 (dispatch)
import qualified Day13.Mod as Day13 (dispatch)
import qualified Day14.Mod as Day14 (dispatch)
import qualified Day15.Mod as Day15 (dispatch)
import qualified Day16.Mod as Day16 (dispatch)
import qualified Day17.Mod as Day17 (dispatch)
import qualified Day18.Mod as Day18 (dispatch)
import qualified Day19.Mod as Day19 (dispatch)
import qualified Day20.Mod as Day20 (dispatch)
import qualified Day21.Mod as Day21 (dispatch)
import qualified Day22.Mod as Day22 (dispatch)
import qualified Day23.Mod as Day23 (dispatch)
import qualified Day24.Mod as Day24 (dispatch)
-- Add day import

import System.Environment (getArgs)

type DayDispatcher = [(Int, IO ())]

dayMap :: [(Int, DayDispatcher)]
dayMap =
  [ -- map of day num to thats days parts
    (1, Day01.dispatch),
    (2, Day02.dispatch),
    (3, Day03.dispatch),
    (4, Day04.dispatch),
    (5, Day05.dispatch),
    (6, Day06.dispatch),
    (7, Day07.dispatch),
    (8, Day08.dispatch),
    (9, Day09.dispatch),
    (10, Day10.dispatch),
    (11, Day11.dispatch),
    (12, Day12.dispatch),
    (13, Day13.dispatch),
    (14, Day14.dispatch),
    (15, Day15.dispatch),
    (16, Day16.dispatch),
    (17, Day17.dispatch),
    (18, Day18.dispatch),
    (19, Day19.dispatch),
    (20, Day20.dispatch),
    (21, Day21.dispatch),
    (22, Day22.dispatch),
    (23, Day23.dispatch),
    (24, Day24.dispatch)
    -- Add day dispatch
  ]

main :: IO ()
main = do
  day : part : _ <- getArgs
  let (Just dayDispatcher) = lookup (read day) dayMap
  let (Just func) = lookup (read part) dayDispatcher
  func
