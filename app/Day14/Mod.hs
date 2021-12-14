module Day14.Mod where

import Data.List
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace
import Utils.Mod

type Polymer = String

type PolymerP2 = Map (Char, Char) Int

type InsertionRuleMap = Map (Char, Char) Char

getPair :: String -> ((Char, Char), Char)
getPair str = ((p1, p2), toInsert)
  where
    [[p1, p2], [toInsert]] = splitOn " -> " str

getPolymerInfo :: [String] -> (Polymer, InsertionRuleMap)
getPolymerInfo s = (polymer, M.fromList (map getPair pairs))
  where
    [[polymer], pairs] = splitOn [""] s

readPolyMerInfo :: IO (Polymer, InsertionRuleMap)
readPolyMerInfo = getPolymerInfo <$> readInputLines

getTransformedPair :: InsertionRuleMap -> (Char, Char) -> String
-- don't add c2 since next pair will always add it
getTransformedPair iMap p@(c1, c2) = case toInsert of
  Just x -> [c1, x]
  Nothing -> [c1]
  where
    toInsert = M.lookup p iMap

checkRules :: InsertionRuleMap -> [(Char, Char)] -> String
checkRules iMap = concatMap (getTransformedPair iMap)

runRules :: InsertionRuleMap -> Polymer -> Polymer
runRules iMap p = checkRules iMap pPairs
  where
    pPairs = pairs p ++ [(last p, ' ')] -- add last element as pair with whitespace so it will be added

runRulesN :: Int -> InsertionRuleMap -> Polymer -> Polymer
runRulesN n iMap p = iterate (runRules iMap) p !! n

getCharCounts :: String -> [Int]
getCharCounts s = map length $ (group . sort) s

part1 :: IO ()
part1 = do
  (p, iMap) <- readPolyMerInfo
  let newP = runRulesN 10 iMap p
  let counts = getCharCounts newP
  let (min, max) = (minimum counts, maximum counts)
  print $ max - min
  return ()

getPolymerInfoP2 :: [String] -> (PolymerP2, InsertionRuleMap)
getPolymerInfoP2 s = (pMap, M.fromList (map getPair pairsLst))
  where
    [[polymer], pairsLst] = splitOn [""] s
    pMap = M.fromListWith (+) [(p, 1) | p <- pairs polymer]

runRulesP2 :: InsertionRuleMap -> PolymerP2 -> PolymerP2
runRulesP2 iMap =
  M.foldrWithKey
    ( \(c1, c2) n ->
        let toInsert = iMap M.! (c1, c2)
         in M.insertWith (+) (c1, toInsert) n . M.insertWith (+) (toInsert, c2) n
    )
    M.empty

counts :: PolymerP2 -> Map Char Int
counts =
  M.map (\n -> (n + 1) `div` 2) -- chars are double counted so half it
    . M.foldrWithKey
      (\(c1, c2) n -> M.insertWith (+) c1 n . M.insertWith (+) c2 n)
      M.empty

runRulesNP2 :: Int -> InsertionRuleMap -> PolymerP2 -> Int
runRulesNP2 n iMap p = maximum c - minimum c
  where
    c = counts $ iterate (runRulesP2 iMap) p !! n

part2 :: IO ()
part2 = do
  (p, iMap) <- getPolymerInfoP2 <$> readInputLines
  print $ p
  print $ runRulesNP2 40 iMap p
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]

-- | Get all pairs of elements in a sliding window
--
-- >>> pairs [1,2,3,4,5]
-- [(1,2),(2,3),(3,4),(4,5)]
pairs :: [b] -> [(b, b)]
pairs xs = zip xs (tail xs)
