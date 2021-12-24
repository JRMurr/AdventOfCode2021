module Day24.Mod where

import Data.Char (digitToInt)
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Utils.Mod

data Val = Lit Int | Var Char deriving (Show, Eq, Ord)

data Expr = Inp Char | Add Char Val | Mul Char Val | Div Char Val | Mod Char Val | Eql Char Val deriving (Show, Eq, Ord)

parseExpr :: String -> Expr
parseExpr s = parseExpr' (words s)
  where
    toVal :: String -> Val
    toVal [c] | c `elem` ['w', 'x', 'y', 'z'] = Var c
    toVal s = Lit (read s)
    parseExpr' :: [String] -> Expr
    parseExpr' ["inp", [v]] = Inp v
    parseExpr' ["add", [v], xs] = Add v (toVal xs)
    parseExpr' ["mul", [v], xs] = Mul v (toVal xs)
    parseExpr' ["div", [v], xs] = Div v (toVal xs)
    parseExpr' ["mod", [v], xs] = Mod v (toVal xs)
    parseExpr' ["eql", [v], xs] = Eql v (toVal xs)
    parseExpr' s = error ("sad: " ++ show s)

type Prog = [Expr]

getProg :: IO Prog
getProg = map parseExpr <$> readInputLines

type Reg = Map Char Int

type ProgState = (Reg, [Int])

runOp :: (Int -> Int -> Int) -> ProgState -> Char -> Val -> ProgState
runOp f (r, inp) var v = (M.insert var fRes r, inp)
  where
    getVal (Lit x) = x
    getVal (Var c) = M.findWithDefault 0 c r
    fRes = f (getVal (Var var)) (getVal v)

runExpr :: ProgState -> Expr -> ProgState
runExpr (r, inp) (Inp v) = let newR = M.insert v (head inp) r in (newR, tail inp)
runExpr s (Add a b) = runOp (+) s a b
runExpr s (Mul a b) = runOp (*) s a b
runExpr s (Div a b) = runOp div s a b
runExpr s (Mod a b) = runOp mod s a b
runExpr s (Eql a b) = runOp (\x y -> if x == y then 1 else 0) s a b

runProg :: Prog -> [Int] -> ProgState
runProg p inp = foldl' runExpr (M.empty, inp) p

runProgTillInput :: Prog -> ProgState -> (Prog, ProgState)
runProgTillInput ((Inp _) : xs) p = (xs, p)
runProgTillInput (x : xs) p = let newP = runExpr p x in runProgTillInput xs newP

-- TODO: either split the prog on inp commands to allow caching of results for each digit
-- or do some branching stuff

-- digits :: Int -> [Int]
-- digits = map digitToInt . show

-- noZeros :: [Int] -> Bool
-- noZeros x = 0 `notElem` x

-- getValidPlates :: [[Int]]
-- getValidPlates = filter noZeros (map digits [ub, (ub -1) .. lb])
--   where
--     getBound :: String -> Int
--     getBound string = read $ concat $ replicate 14 string
--     lb = getBound "1"
--     ub = getBound "9"

-- checkPlate :: Prog -> [Int] -> Bool
-- checkPlate p inp = M.lookup 'z' res == Just 0
--   where
--     (res, _) = runProg p inp

part1 :: IO ()
part1 = do
  input <- getProg
  -- print $ find (checkPlate input) getValidPlates
  return ()

part2 :: IO ()
part2 = do
  input <- getProg
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]