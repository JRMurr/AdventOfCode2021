module Day21.Mod where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List
import Data.List.Split
import Debug.Trace
import Utils.Mod

getStart :: String -> Player
getStart s = P {score = 0, pos = read start}
  where
    [_, start] = splitOn "starting position: " s

data Player = P {score :: Int, pos :: Int} deriving (Show, Eq, Ord)

type Players = IntMap Player

parseStarts :: [String] -> Players
parseStarts strs = M.fromList $ enumerate $ map getStart strs

readStarts :: IO Players
readStarts = parseStarts <$> readInputLines

-- https://en.wikipedia.org/wiki/Modulo_operation#Modulo_with_offset
getBoardPos :: Int -> Int
getBoardPos a = a - (n * ((a - d) `div` n))
  where
    n = 10
    d = 1

detDice :: [Int]
detDice = cycle [1 .. 100]

getRoll :: Int -> Int
getRoll n = sum $ take 3 $ drop start detDice
  where
    start = (n) * 3

movePlayer :: Int -> Player -> Player
movePlayer amount P {score = score, pos = pos} = newP -- traceShow ("startP: ", pos, "amount", amount, "player:", newP)
  where
    newPos = getBoardPos (pos + amount)
    newP = P {score = score + newPos, pos = newPos}

stepPlayer :: Players -> Int -> Int -> Players
stepPlayer pMap playerNum rollNum = M.adjust (movePlayer roll) playerNum pMap
  where
    roll = getRoll rollNum

checkWin :: Players -> Maybe (Player, Player)
checkWin pMap = case partition isWin players of
  ([winner], [loser]) -> Just (winner, loser)
  _ -> Nothing
  where
    players = M.elems pMap
    isWin p = score p >= 1000

-- run game until a player has 1000 score, return (numRolls, winner, loser)
runGame :: Players -> (Int, Player, Player)
runGame = runGame' 0
  where
    playerNums = cycle [0, 1]
    runGame' :: Int -> Players -> (Int, Player, Player)
    runGame' rollNum pMap = case checkWin newPlayers of
      Just (winner, loser) -> ((rollNum + 1) * 3, winner, loser)
      Nothing -> runGame' (rollNum + 1) newPlayers
      where
        playerNum = playerNums !! rollNum
        newPlayers = stepPlayer pMap playerNum rollNum

part1 :: IO ()
part1 = do
  input <- readStarts
  print input
  let (rollNums, _, P {score = score}) = runGame input
  print $ rollNums * score
  return ()

-- return the player number of the winner if there is one
checkWinP2 :: Players -> Maybe Int
checkWinP2 pMap = find isWin [0, 1]
  where
    isWin num = score ((M.!) pMap num) >= 21

type GameState = (Int, Players) -- rollNumber and state of players

allDiceRolls :: [Int]
allDiceRolls = [sum [x, y, z] | x <- baseRoll, y <- baseRoll, z <- baseRoll]
  where
    baseRoll = [1, 2, 3]

getAllNewStates :: GameState -> [GameState]
getAllNewStates (rollNum, pMap) = map (\roll -> (rollNum + 1, M.adjust (movePlayer roll) playerNum pMap)) allDiceRolls
  where
    playerNums = cycle [0, 1]
    playerNum = playerNums !! rollNum

runGameP2 :: [GameState] -> (Int, Int) -> (Int, Int)
runGameP2 [] res = res
runGameP2 (curr@(rollNum, pMap) : xs) wins@(p1Win, p2Win) = case checkWinP2 pMap of
  Just (0) -> runGameP2 xs (p1Win + 1, p2Win)
  Just 1 -> runGameP2 xs (p1Win, p2Win + 1)
  Just _ -> error "invalid winner"
  Nothing -> runGameP2 (newStates ++ xs) wins
  where
    newStates = getAllNewStates curr

part2 :: IO ()
part2 = do
  input <- readStarts
  print input
  print $ runGameP2 [(0, input)] (0, 0)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]