module Day07.Mod where

import Data.List.Split (splitOn)
import Utils.Mod

getFuelCost :: (Int, [Int]) -> Int
getFuelCost (target, rest) = sum $ map (\x -> abs (target - x)) rest

getMinFuelCost :: [Int] -> Int
getMinFuelCost lst = minimum $ map getFuelCost $ pickOne lst

part1 :: IO ()
part1 = do
  input <- parseIntsWithSep "," <$> getRawInput
  print $ getMinFuelCost input
  return ()

gausSum :: Integral a => a -> a
gausSum n = (n * (n + 1)) `div` 2

getMinMax :: (Foldable t, Ord b) => t b -> (b, b)
getMinMax lst = (minimum lst, maximum lst)

getFuelCostPart2 :: Int -> [Int] -> Int
getFuelCostPart2 target rest = sum $ map (\x -> gausSum $ abs (target - x)) rest

getMinFuelCostPart2 :: [Int] -> Int
getMinFuelCostPart2 lst = minimum [getFuelCostPart2 x lst | x <- [minVal .. maxVal]]
  where
    (minVal, maxVal) = getMinMax lst

part2 :: IO ()
part2 = do
  input <- parseIntsWithSep "," <$> getRawInput
  print $ getMinFuelCostPart2 input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]