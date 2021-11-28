module Day02.Mod where

import PseudoMacros (__FILE__)
import Utils.Mod (getInputFile, readInput)

part1 :: IO ()
part1 = do
  input <- readInput $ getInputFile $__FILE__
  putStrLn "part1"
  return ()

part2 :: IO ()
part2 = do
  input <- readInput $ getInputFile $__FILE__
  putStrLn "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]