module Main where

-- need to update other-modules to add sub modules
import qualified Day01.Mod as Day01 (dispatch)
import System.Environment (getArgs)

type DayDispatcher = [(Int, IO ())]

dayMap :: [(Int, DayDispatcher)]
dayMap = [(1, Day01.dispatch)]

main :: IO ()
main = do
  [dayNumber, part] <- getArgs
  let (Just dayDispatcher) = lookup (read dayNumber) dayMap
  let (Just func) = lookup (read part) dayDispatcher
  func
