module Day01.Mod where

import PseudoMacros (__FILE__)
import Utils.Mod

-- Get all pairs of elements in a sliding window
-- >>> pairs [1,2,3,4,5]
-- [(1,2),(2,3),(3,4),(4,5)]
pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)

-- Partition a list with a sliding window of passed size
-- >>> partition' 3 1 [1,2,3,4,5,6]
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
partition' :: Int -> Int -> [a] -> [[a]]
partition' size offset
  | size <= 0 = error "partition': size must be positive"
  | offset <= 0 = error "partition': offset must be positive"
  | otherwise = loop
  where
    loop :: [a] -> [[a]]
    loop xs = case splitAt size xs of
      -- If the second part is empty, we're at the end. But we might
      -- have gotten less than we asked for, hence the check.
      (ys, []) -> [ys | length ys == size]
      (ys, _) -> ys : loop (drop offset xs)

part1 :: IO ()
part1 = do
  input <- readInputLinesInteger $ getInputFile $__FILE__
  let paired = pairs input
  print $ count (\(a, b) -> b > a) paired
  return ()

toThruple :: [c] -> (c, c, c)
toThruple [a, b, c] = (a, b, c)
toThruple _ = error "invalid size"

threeSum :: Num a => (a, a, a) -> a
threeSum (a, b, c) = a + b + c

part2 :: IO ()
part2 = do
  input <- readInputLinesInteger $ getInputFile $__FILE__
  let threes = map toThruple $ partition' 3 1 input
  print $ count (\(a, b) -> threeSum b > threeSum a) (pairs threes)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
