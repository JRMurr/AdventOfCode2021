module Day08.Mod where

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Utils.Mod

type SignalPattern = String

-- First entry is signal patterns, second is the output
type Entry = ([SignalPattern], [SignalPattern])

readEntry :: String -> Entry
readEntry line = (toPattern patterns, toPattern output)
  where
    toPattern str = words str
    [patterns, output] = splitOn " | " line

type SignalMap = Map SignalPattern Int

addPat :: SignalMap -> SignalPattern -> Maybe Int -> SignalMap
addPat m _ Nothing = m
addPat m key (Just val) = Map.insert key val m

handleUniqueNum :: SignalPattern -> Maybe Int
handleUniqueNum pat
  | patLen == 2 = Just 1
  | patLen == 4 = Just 4
  | patLen == 3 = Just 7
  | patLen == 7 = Just 8
  | otherwise = Nothing -- Need more complex handling to determine which number this pattern is
  where
    patLen = length pat

findUniqueNum :: Entry -> SignalMap -> SignalMap
findUniqueNum (pat, output) map = foldr (\p m -> addPat m p (handleUniqueNum p)) map allPats
  where
    allPats = pat ++ output

getAllUniqueNums :: [Entry] -> SignalMap
getAllUniqueNums = foldr findUniqueNum Map.empty

countUniqueNums :: SignalMap -> [Entry] -> Int
countUniqueNums m entries = sum $ map countEntry entries
  where
    countEntry :: Entry -> Int
    countEntry (_, output) = count (`Map.member` m) output

part1 :: IO ()
part1 = do
  input <- map readEntry <$> readInputLines
  let uniqueNums = getAllUniqueNums input
  -- print $ uniqueNums
  print $ countUniqueNums uniqueNums input
  return ()

data SegmentLineParts = Top | Bottom | Middle | TopLeft | TopRight | BottomLeft | BottomRight deriving (Show, Eq, Ord)

type SegmentDisplay = Set SegmentLineParts

intToDisplay :: Int -> SegmentDisplay
intToDisplay 0 = Set.fromList [Top, Bottom, TopLeft, TopRight, BottomLeft, BottomRight]
intToDisplay 1 = Set.fromList [TopRight, BottomRight]
intToDisplay 2 = Set.fromList [Top, Bottom, Middle, TopRight, BottomLeft]
intToDisplay 3 = Set.fromList [Top, Bottom, Middle, TopRight, BottomRight]
intToDisplay 4 = Set.fromList [TopLeft, TopRight, Middle, BottomRight]
intToDisplay 5 = Set.fromList [Top, Bottom, Middle, TopLeft, BottomRight]
intToDisplay 6 = Set.fromList [Top, Bottom, Middle, TopLeft, BottomLeft, BottomRight]
intToDisplay 7 = Set.fromList [Top, TopRight, BottomRight]
intToDisplay 8 = Set.fromList [Top, Bottom, Middle, TopLeft, TopRight, BottomLeft, BottomRight]
intToDisplay 9 = Set.fromList [Top, Bottom, Middle, TopLeft, TopRight, BottomRight]
intToDisplay _ = error "invalid number"

displayToInt :: SegmentDisplay -> Maybe Int
displayToInt s = listToMaybe [i | i <- [0 .. 9], s == intToDisplay i]

patToPossibleNums :: SignalPattern -> Set Int
patToPossibleNums pat
  | patLen == 2 = Set.singleton 1
  | patLen == 4 = Set.singleton 4
  | patLen == 3 = Set.singleton 7
  | patLen == 7 = Set.singleton 8
  | patLen == 5 = Set.fromList [2, 3, 5]
  | patLen == 6 = Set.fromList [0, 6, 9]
  where
    patLen = length pat

-- go over every SignalPattern, map each char to a set of what SegmentLineParts it could be
type CharToPossibleLines = Map Char (Set SegmentLineParts)

sigPatternToPossibleSegmentParts :: SignalPattern -> CharToPossibleLines
sigPatternToPossibleSegmentParts p = Map.fromList [(c, possibleLineParts) | c <- p]
  where
    possibleNums = patToPossibleNums p
    possibleSegmentDisplays = Set.map intToDisplay possibleNums
    possibleLineParts = foldr1 Set.union possibleSegmentDisplays -- union all possible segments since the char could be in any one of the numbers

joinEntry :: Entry -> [SignalPattern]
joinEntry (s, o) = s ++ o

joinMaps :: CharToPossibleLines -> CharToPossibleLines -> CharToPossibleLines
joinMaps = Map.unionWith Set.intersection

-- returns a map of char to possible postions in the segment display that char could be
entryToPossibleSegmentPart :: Entry -> CharToPossibleLines
entryToPossibleSegmentPart e = foldr (\p m -> joinMaps m (sigPatternToPossibleSegmentParts p)) Map.empty (joinEntry e)

type CharToLine = Map Char SegmentLineParts

getSegment :: CharToLine -> SignalPattern -> SegmentDisplay
getSegment charMap pat = Set.fromList $ map (charMap Map.!) pat

getMapping :: [SignalPattern] -> CharToPossibleLines -> CharToLine
getMapping patterns possibleLines =
  case helperRes of
    Just res -> res
    Nothing -> error "could not get map"
  where
    helperRes = getMapping' patterns possibleLines Map.empty (Map.keys possibleLines)

isValidAssignment :: [SignalPattern] -> CharToLine -> Bool
isValidAssignment patterns charMap = all (isJust . displayToInt) segments
  where
    segments = map (getSegment charMap) patterns

getMapping' :: [SignalPattern] -> CharToPossibleLines -> CharToLine -> [Char] -> Maybe CharToLine
getMapping' patterns _ charToLine [] = if isValidAssignment patterns charToLine then Just charToLine else Nothing
getMapping' patterns possibleLines charToLine (c : xs) =
  case availableLines of
    [] -> Nothing
    lines -> listToMaybe $ mapMaybe recursiveWith lines
  where
    availableLines = getMappingForChar possibleLines charToLine c
    assignLine l = Map.insert c l charToLine
    recursiveWith l = getMapping' patterns possibleLines (assignLine l) xs
    tmp lines = mapMaybe recursiveWith lines

getMappingForChar :: CharToPossibleLines -> CharToLine -> Char -> [SegmentLineParts]
getMappingForChar possibleLinesMap charToLine c = availableLines
  where
    possibleLines = possibleLinesMap Map.! c
    takenLines = Set.fromList $ Map.elems charToLine
    availableLines = Set.elems $ Set.difference possibleLines takenLines

patternToNum :: CharToLine -> SignalPattern -> Int
-- trace ("pat: " ++ pat ++ " segment: " ++ show segment ++ " map: " ++ show charMap) $
patternToNum charMap pat = fromJust $ displayToInt segment
  where
    segment = Set.fromList $ map (charMap Map.!) pat

joinDigits :: [Int] -> Int
joinDigits lst = foldl (\sum (idx, val) -> sum + val * (10 ^ idx)) 0 (zip [0 .. (length lst)] (reverse lst))

getOutputVal :: Entry -> Int
getOutputVal e@(_, output) = joinDigits outputNums
  where
    possibleSegments = entryToPossibleSegmentPart e
    charMapping = getMapping (joinEntry e) possibleSegments
    outputNums = map (patternToNum charMapping) output

part2 :: IO ()
part2 = do
  input <- map readEntry <$> readInputLines
  print $ sum $ map getOutputVal input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]