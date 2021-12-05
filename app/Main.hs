module Main where

-- need to update other-modules to add sub modules
import qualified Day01.Mod as Day01 (dispatch)
import qualified Day02.Mod as Day02 (dispatch)
import qualified Day03.Mod as Day03 (dispatch)
import qualified Day04.Mod as Day04 (dispatch)
import qualified Day05.Mod as Day05 (dispatch)
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
    (5, Day05.dispatch)
    -- Add day dispatch
  ]

main :: IO ()
main = do
  day : part : _ <- getArgs
  let (Just dayDispatcher) = lookup (read day) dayMap
  let (Just func) = lookup (read part) dayDispatcher
  func
