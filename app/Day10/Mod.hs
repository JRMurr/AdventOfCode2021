module Day10.Mod where

import Data.List
import Data.Maybe
import Debug.Trace
import Utils.Mod

newtype Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x : xs)) = (Just x, Stack xs)

-- Open Close
type Pair = (Char, Char)

validPairs :: [Pair]
validPairs = [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

isMatch :: Char -> Char -> Bool
isMatch lastOpen currClose = (lastOpen, currClose) `elem` validPairs

isClose :: Char -> Bool
isClose c = c `elem` closers
  where
    closers = map snd validPairs

-- Only checks if the string has a bad closing brace, Incomplete lines would return true
-- isCorrupted :: String -> Bool
-- isCorrupted str = isCorrupted' str empty
--   where
--     isCorrupted' :: String -> Stack Char -> Bool
--     isCorrupted [] stack

-- Return the stack if not corrupted or the end char that was invalid
data CheckRes = S (Stack Char) | C Char

checkLine :: String -> Stack Char -> CheckRes
checkLine [] stack = S stack
checkLine (curr : xs) stack | isClose curr = case poppedChar of
  Just lastOpen -> if isMatch lastOpen curr then checkLine xs restStack else C curr
  Nothing -> S stack
  where
    (poppedChar, restStack) = pop stack
checkLine (curr : xs) stack = checkLine xs (push curr stack)

getScore :: String -> Int
getScore str = case checkRes of
  S _ -> 0 -- incomplete line
  C ')' -> 3
  C ']' -> 57
  C '}' -> 1197
  C '>' -> 25137
  where
    checkRes = checkLine str empty

part1 :: IO ()
part1 = do
  input <- readInputLines
  print $ sum $ map getScore input
  return ()

charValue :: Char -> Int
charValue '(' = 1
charValue '[' = 2
charValue '{' = 3
charValue '<' = 4
charValue _ = 0

getStackValue :: Stack Char -> Int
getStackValue (Stack vals) = foldl (\score char -> (score * 5) + charValue char) 0 vals

completeLine :: String -> Maybe Int
completeLine str = case checkRes of
  C _ -> Nothing -- corrupted
  S stack -> Just (getStackValue stack)
  where
    checkRes = checkLine str empty

part2 :: IO ()
part2 = do
  input <- readInputLines
  input <- readInputLines
  let scores = sort $ mapMaybe completeLine input
  let middleIdx = length scores `div` 2 -- fine to div 2 since list is odd
  print $ scores
  print $ middleIdx
  print $ scores !! middleIdx
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]