module Day04.Mod where

import Data.Char (isSpace)
import Data.List (inits, partition)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Utils.Coords
import Utils.Mod

-- | Board is a hash map of coordinate to the value at that position
type Board = Map Coord Int

splitOnBlank :: [String] -> [[String]]
splitOnBlank = splitOn [""]

toRow :: String -> [Int]
toRow str = map read (words str)

toBoard :: [String] -> Board
toBoard rows = Map.fromList (coordLinesInt splitRows)
  where
    splitRows = map toRow rows

parseBingoNums :: [String] -> [Int]
parseBingoNums [x] = map read (splitOn "," x) :: [Int]
parseBingoNums _ = error "invalid input"

parseInput :: [String] -> ([Int], [Board])
parseInput input = (parseBingoNums bingoNumStr, boards)
  where
    bingoNumStr : xs = splitOnBlank input
    boards = map toBoard xs

-- follows direction in board, if 5 elems are in that dir returns true
isDirFull :: Int -> DirFunc -> Coord -> Board -> Bool
isDirFull 5 _ _ _ = True
isDirFull count dir currCord board = Map.member currCord board && isDirFull (count + 1) dir nextCord board
  where
    nextCord = dir currCord

topRow :: [Coord]
topRow = [C 0 x | x <- [0 .. 5]]

leftCol :: [Coord]
leftCol = [C x 0 | x <- [0 .. 5]]

-- | Returns true if given the numbers if the board is done (row, col, or diag is filled)
isBoardDone :: [Int] -> Board -> Bool
isBoardDone nums board =
  Map.size matchedBoard >= 5
    && (isSouthEastDiag || isNorthEast || isVert || isRow)
  where
    -- filter board to only show cords with values in nums
    matchedBoard = Map.filter (`elem` nums) board
    isSouthEastDiag = isDirFull 0 southEast (C 0 0) matchedBoard
    isNorthEast = isDirFull 0 northEast (C 0 0) matchedBoard
    isVert = any (\c -> isDirFull 0 below c matchedBoard) topRow
    isRow = any (\c -> isDirFull 0 right c matchedBoard) leftCol

-- | Returns the score of a done board
getBoardScore :: Board -> [Int] -> Int
getBoardScore board nums = unMatchedSum * lastNum
  where
    unMatchedBoard = Map.filter (`notElem` nums) board
    lastNum = last nums
    unMatchedSum = Map.foldr (+) 0 unMatchedBoard

getDoneBoardScore :: [Board] -> [[Int]] -> Int
getDoneBoardScore _ [] = error "no board is valid"
getDoneBoardScore boards (nums : xs) = case doneBoards of
  [board] -> getBoardScore board nums
  _ -> getDoneBoardScore boards xs
  where
    doneBoards = filter (isBoardDone nums) boards

part1 :: IO ()
part1 = do
  input <- readInputLines
  let (nums, boards) = parseInput input
  print $ getDoneBoardScore boards (inits nums)
  return ()

partitionByDone :: [Int] -> [Board] -> ([Board], [Board])
partitionByDone nums = partition (isBoardDone nums)

getNumsTillDoneAndScore :: [[Int]] -> Board -> Int
getNumsTillDoneAndScore [] _ = error "no nums left"
getNumsTillDoneAndScore (currNums : xs) board =
  if isBoardDone currNums board
    then getBoardScore board currNums
    else getNumsTillDoneAndScore xs board

getLastDoneBoardScore :: [Board] -> [[Int]] -> Int
getLastDoneBoardScore _ [] = error "no board is valid"
getLastDoneBoardScore [board] (nums : xs) = getNumsTillDoneAndScore (nums : xs) board
getLastDoneBoardScore boards (nums : xs) = getLastDoneBoardScore notDoneBoards xs
  where
    (_, notDoneBoards) = partitionByDone nums boards

part2 :: IO ()
part2 = do
  input <- readInputLines
  let (nums, boards) = parseInput input
  print $ getLastDoneBoardScore boards (inits nums)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]