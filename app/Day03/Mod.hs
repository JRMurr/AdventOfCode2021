module Day03.Mod where

import Debug.Trace
import Utils.Mod

type BinNum = [Int]

type BitCounter = [BinNum] -> Int -> Int

charToInt :: Char -> Int
charToInt '1' = 1
charToInt _ = 0

toBins :: [String] -> [BinNum]
toBins = map (map charToInt)

countOnesInPosition :: Int -> [BinNum] -> Int
countOnesInPosition idx = count (\binNum -> binNum !! idx == 1)

compareCounts :: [BinNum] -> Int -> (Int -> Int -> Bool) -> Int
compareCounts binNum idx compareFunc = if onesCount `compareFunc` zeroCount then 1 else 0
  where
    onesCount = countOnesInPosition idx binNum
    zeroCount = length binNum - onesCount

getMostCommonBit :: BitCounter
getMostCommonBit binNum idx = compareCounts binNum idx (>=)

getLeastCommonBit :: BitCounter
getLeastCommonBit binNum idx = compareCounts binNum idx (<)

toDecimal :: BinNum -> Int
toDecimal bits = foldl (\sum (idx, val) -> sum + val * (2 ^ idx)) 0 (zip [0 .. (length bits)] (reverse bits))

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