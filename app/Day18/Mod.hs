module Day18.Mod where

import Control.Applicative (Alternative ((<|>)))
import Data.Bifunctor (Bifunctor (second))
import Data.Char (digitToInt, isDigit)
import Utils.Mod

data SNum = Single Int | P SNum SNum

instance Show SNum where
  show (Single n) = show n
  show (P l r) = "[" ++ show l ++ "," ++ show r ++ "]"

type FlatSNum = [(Int, Int)] -- (value, depth)

-- parseSNum :: String -> (SNum, String)
-- parseSNum [] = error "Empty!"
-- parseSNum (d : xs) | isDigit d = (Single (digitToInt d), xs)
-- parseSNum (',' : xs) = parseSNum xs
-- parseSNum ('[' : xs) = (P e1 e2, tail r2) -- tail to remove the ']'
--   where
--     (e1, r1) = parseSNum xs
--     (e2, r2) = parseSNum r1
-- parseSNum xs = error ("sad:" ++ xs)

parseFSNum :: Int -> String -> (FlatSNum, String)
parseFSNum _ [] = error "Empty!"
parseFSNum n (d : xs) | isDigit d = ([(digitToInt d, n)], xs)
parseFSNum n (',' : xs) = parseFSNum n xs
parseFSNum n ('[' : xs) = (e1 ++ e2, tail r2) -- tail to remove the ']'
  where
    (e1, r1) = parseFSNum (n + 1) xs
    (e2, r2) = parseFSNum (n + 1) r1

parseFSNums :: IO [FlatSNum]
parseFSNums = map (fst . parseFSNum 0) <$> readInputLines

tryExplode :: FlatSNum -> Maybe FlatSNum
tryExplode ((p1, d1) : (p2, d2) : (v, d') : xs) -- case when explode on left most pair
  | d1 > 4 && d1 == d2 =
    Just $ (0, d1 -1) : (p2 + v, d') : xs
tryExplode [(v, d'), (p1, d1), (p2, d2)] -- explode on right most pair
  | d1 > 4 && d1 == d2 =
    Just [(v + p1, d'), (0, d1 -1)]
tryExplode ((v, d) : (p1, d1) : (p2, d2) : (v1, d') : xs) -- explode on a "middle" pair
  | d1 > 4 && d1 == d2 = Just $ (v + p1, d) : (0, d1 -1) : (p2 + v1, d') : xs
tryExplode (x : xs) = (x :) <$> tryExplode xs -- no explode go through rest of list
tryExplode [] = Nothing -- no explode happened

-- half the input and return tuple where first elm is rounded down the second is rounded up
halfRoundBoth :: Int -> (Int, Int)
halfRoundBoth n = case n `quotRem` 2 of
  (half, 0) -> (half, half)
  (half, _) -> (half, half + 1)

trySplit :: FlatSNum -> Maybe FlatSNum
trySplit [] = Nothing
trySplit ((v, d) : xs) | v >= 10 = let (v1, v2) = halfRoundBoth v in Just $ (v1, d + 1) : (v2, d + 1) : xs
trySplit (x : xs) = (x :) <$> trySplit xs

reduce :: FlatSNum -> FlatSNum
reduce sNum = maybe sNum reduce $ tryExplode sNum <|> trySplit sNum

treeify :: FlatSNum -> SNum
treeify = fst . treeify' 0
  where
    treeify' d all@((_, d') : _) | d < d' = (P l r, rest')
      where
        treeify1 = treeify' (d + 1)
        (l, r1) = treeify1 all
        (r, rest') = treeify1 r1
    treeify' d ((n, d') : rest) | d == d' = (Single n, rest)
    treeify' d x = error ("sad: " ++ show (d, x))

-- join lists and increase all depths
add :: FlatSNum -> FlatSNum -> FlatSNum
add m n = reduce . map (second (+ 1)) $ m ++ n

-- Adds all elements of a list together to produce single num
addList :: [FlatSNum] -> FlatSNum
addList = foldl1 add

magnitude :: SNum -> Int
magnitude (Single n) = n
magnitude (P x y) = (3 * magnitude x) + (2 * magnitude y)

part1 :: IO ()
part1 = do
  fSNums <- parseFSNums
  let added = addList fSNums
  print $ magnitude . treeify $ added
  return ()

addGetMag :: FlatSNum -> FlatSNum -> Int
addGetMag n m = magnitude . treeify $ add n m

part2 :: IO ()
part2 = do
  fSNums <- parseFSNums
  print $ maximum [addGetMag x y | (x, y) <- cartProd fSNums fSNums]
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]