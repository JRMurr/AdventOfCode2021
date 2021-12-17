module Day17.Mod where

import Utils.Mod

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