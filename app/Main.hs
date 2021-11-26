module Main where

-- need to update other-modules to add sub modules
import Day01.Mod (part1, part2)
import System.Environment (getArgs)

dispatch :: [(String, IO ())]
dispatch = [("part1", part1), ("part2", part2)]

main :: IO ()
main = do
  func_name <- getArgs
  let (Just func) = lookup (head func_name) dispatch
  func
