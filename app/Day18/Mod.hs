module Day18.Mod where

import Data.Char (digitToInt, isDigit)
import Utils.Mod

data Element = Single Int | P Pair deriving (Show, Eq)

type Pair = (Element, Element)

parseExpr :: String -> (Element, String)
parseExpr [] = error "Empty!"
parseExpr (d : xs) | isDigit d = (Single (digitToInt d), xs)
parseExpr (',' : xs) = parseExpr xs
parseExpr ('[' : xs) = (P (e1, e2), tail r2)
  where
    (e1, r1) = parseExpr xs
    (e2, r2) = parseExpr r1
parseExpr xs = error ("sad:" ++ xs)

getPair :: String -> Pair
getPair s = case parseExpr s of
  (P pair, _) -> pair
  _ -> error ("not a pair: " ++ show s)

getAllPairs :: IO [Pair]
getAllPairs = map getPair <$> readInputLines

part1 :: IO ()
part1 = do
  pairs <- getAllPairs
  print pairs
  return ()

part2 :: IO ()
part2 = do
  input <- readInputLines
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]