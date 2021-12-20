module Day03.Mod where

import Data.List
import Debug.Trace
import Utils.Mod

type BitCounter = [BinNum] -> Int -> Int

charToInt :: Char -> Int
charToInt '1' = 1
charToInt _ = 0

toBins :: [String] -> [BinNum]
toBins = map (map charToInt)

countOnesInPosition :: Int -> [BinNum] -> Int
countOnesInPosition idx = count (\binNum -> binNum !! idx == 1)

-- | Return a tuple whos first element is all binNums with the value in idx =1, and the send element who idx is 0
--
-- >>> partitionByIdx 0 [[1,0,0], [1,0,1], [0,1,0], [0,1,1]]
-- ([[1,0,0],[1,0,1]],[[0,1,0],[0,1,1]])
partitionByIdx :: Int -> [BinNum] -> ([BinNum], [BinNum])
partitionByIdx idx = partition (\binNum -> binNum !! idx == 1)

compareCounts :: [BinNum] -> Int -> (Int -> Int -> Bool) -> Int
compareCounts binNums idx compareFunc = if onesCount `compareFunc` zeroCount then 1 else 0
  where
    (onesNums, zerosNums) = partitionByIdx idx binNums
    (onesCount, zeroCount) = (length onesNums, length zerosNums)

getMostCommonBit :: BitCounter
getMostCommonBit binNum idx = compareCounts binNum idx (>=)

getLeastCommonBit :: BitCounter
getLeastCommonBit binNum idx = compareCounts binNum idx (<)

part1 :: IO ()
part1 = do
  input <- readInputLines
  let bitLen = length $ head input
  let bins = toBins input
  let gamma = [getMostCommonBit bins idx | idx <- [0 .. (bitLen -1)]]
  let epsilon = [getLeastCommonBit bins idx | idx <- [0 .. (bitLen - 1)]]
  print $ toDecimal gamma * toDecimal epsilon
  return ()

doesIdxMatch :: BinNum -> Int -> Int -> Bool
doesIdxMatch binNum idx bitVal = binNum !! idx == bitVal

filterByIdx :: [BinNum] -> Int -> Int -> [BinNum]
filterByIdx binNums idx bitVal = filter (\str -> doesIdxMatch str idx bitVal) binNums

getRating :: [BinNum] -> Int -> BitCounter -> BinNum
getRating [] idx _ = error ("empty list " ++ show idx)
getRating [binNum] _ _ = binNum
getRating binNums idx counterFunc = getRating filtered (idx + 1) counterFunc
  where
    bitVal = counterFunc binNums idx
    filtered = filterByIdx binNums idx bitVal

getOxGen :: [BinNum] -> BinNum
getOxGen binNums = getRating binNums 0 getMostCommonBit

getCo2 :: [BinNum] -> BinNum
getCo2 binNums = getRating binNums 0 getLeastCommonBit

part2 :: IO ()
part2 = do
  input <- readInputLines
  let bins = toBins input
  let ox = getOxGen bins
  let co2 = getCo2 bins
  print $ toDecimal ox * toDecimal co2
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
