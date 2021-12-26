{-# LANGUAGE BlockArguments #-}

module Day24.Mod where

import Control.Monad
import Control.Monad.State
import Data.Char (digitToInt, intToDigit)
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
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
-- runExpr (r, inp) (Inp v) = let newR = M.insert v (head inp) r in (newR, tail inp)
runExpr (r, inp) (Inp v) = error "handle in find"
runExpr s (Add a b) = runOp (+) s a b
runExpr s (Mul a b) = runOp (*) s a b
runExpr s (Div a b) = runOp div s a b
runExpr s (Mod a b) = runOp mod s a b
runExpr s (Eql a b) = runOp (\x y -> if x == y then 1 else 0) s a b

runProg :: Prog -> [Int] -> ProgState
runProg p inp = foldl' runExpr (M.empty, inp) p

-- runProgTillInput :: Prog -> ProgState -> (Prog, ProgState)
-- runProgTillInput ((Inp _) : xs) p = (xs, p)
-- runProgTillInput (x : xs) p = let newP = runExpr p x in runProgTillInput xs newP

-- digits :: Int -> [Int]
-- digits = map digitToInt . show

-- (a,b,c)
type StepVars = (Int, Int, Int)

toStep :: [Expr] -> StepVars
toStep x = (a, b, c)
  where
    Div 'z' (Lit a) = x !! 4
    Add 'x' (Lit b) = x !! 5
    Add 'y' (Lit c) = x !! 15

getSteps :: IO [StepVars]
getSteps = map toStep . chunksOf 18 <$> getProg

runStep :: StepVars -> Int -> Int -> Int
runStep (a, b, c) w z = if x /= w then (z' * 26) + (w + c) else z'
  where
    x = (z `mod` 26) + b
    z' = z `quot` a

solve :: [Int] -> Int -> [StepVars] -> State (S.Set (Int, Int)) [[Int]]
solve ws z (s : steps) = do
  cache <- get
  if S.member (z, length steps) cache -- since we go over all possible digits in each call if we have seen z with this many steps remaining we can exit
    then pure []
    else do
      result <- concat <$> traverse recCall ws
      modify (S.insert (z, length steps))
      pure result
  where
    recCall w =
      let z' = runStep s w z
       in if null steps
            then pure [[w] | z' == 0]
            else map (w :) <$> solve ws z' steps

run :: [StepVars] -> [Int] -> String
run steps ws = map intToDigit $ head $ evalState (solve ws 0 steps) S.empty

part1 :: IO ()
part1 = do
  input <- getSteps

  putStrLn $ run input [9, 8 .. 1]

  return ()

part2 :: IO ()
part2 = do
  input <- getSteps
  putStrLn $ run input [1 .. 9]

  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]