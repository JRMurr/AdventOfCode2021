module Day01.Mod where

import Utils.Mod

-- | Get all pairs of elements in a sliding window
--
-- >>> pairs [1,2,3,4,5]
-- [(1,2),(2,3),(3,4),(4,5)]
pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)

-- | Partition a list with a sliding window of passed size
--
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
      -- have gotten less than we asked for. IF so don't return it
      (ys, []) -> [ys | length ys == size]
      (ys, _) -> ys : loop (drop offset xs)

-- | Returns true if the second elem of the tuple is greater than the first
--
-- >>> isGreater (1,2)
-- True
--
-- >>> isGreater (2,1)
-- False
isGreater :: Ord a => (a, a) -> Bool
isGreater (a, b) = b > a

part1 :: IO ()
part1 = do
  input <- readInputLinesInteger
  let paired = pairs input
  print $ count isGreater paired
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLinesInteger
  let threes = map sum (partition' 3 1 input)
  print $ count isGreater (pairs threes)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
