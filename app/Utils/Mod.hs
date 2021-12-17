module Utils.Mod where

import Data.List
-- import System.FilePath (combine, takeDirectory)

import Data.List.Split (splitOn)
import Data.Void
import System.Environment (getArgs)
import Text.Megaparsec (Parsec, anySingle, eof, manyTill, parse, satisfy, sepBy, setInput)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf

readInputLines :: IO [String]
readInputLines = lines <$> getRawInput

readInputLinesInteger :: IO [Integer]
readInputLinesInteger = map read . lines <$> getRawInput

removeEmptyString :: [String] -> [String]
removeEmptyString = filter (not . null)

readNormalInput :: String -> IO String
readNormalInput day = readFile (printf "app/Day%02d/in" (read day :: Integer))

readExampleInput :: String -> IO String
readExampleInput day = readFile (printf "app/Day%02d/in.example" (read day :: Integer))

-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: IO String
getRawInput =
  do
    day : _ : xs <- getArgs
    case xs of
      [] -> readNormalInput day
      "0" : _ -> readNormalInput day
      "1" : _ -> readExampleInput day
      "-" : _ -> getContents
      fileName : _ -> getRawInput

parseIntsWithSep :: String -> String -> [Int]
parseIntsWithSep sep str = map read $ splitOn sep str

-- https://github.com/glguy/advent2019/blob/master/common/Advent.hs

type Parser = Parsec Void String

getParsedInput :: FilePath -> Parser a -> IO a
getParsedInput path p =
  do
    input <- readFile path
    case parse p "input" input of
      Left e -> fail (errorBundlePretty e)
      Right a -> return a

readParsedLines :: FilePath -> Parser a -> IO [a]
readParsedLines path p = do
  file <- readFile path
  return (getParsedLines file p)

-- | Run a parser with 'parseLines' on the input file.
getParsedLines :: String -> Parser a -> [a]
getParsedLines input p =
  case parseLines p input of
    Left string -> error string
    Right res -> res

-- | Run a parser on each line of the input file. Each line will be parsed
-- in isolation. The parser must consume the whole line.
--
-- >>> parseLines (Control.Applicative.many anySingle) "12\n34\n"
-- Right ["12","34"]
-- >>> parseLines number "12\n34\n"
-- Right [12,34]
parseLines :: Parser a -> String -> Either String [a]
parseLines p input =
  case parse (traverse parse1 (lines input)) "input" input of
    Left e -> Left (errorBundlePretty e)
    Right a -> Right a
  where
    parse1 x = setInput x *> p <* eof <* setInput "\n" <* newline

-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a, b) -> a : chunks n b

-- | Returns a list of ways to select an element from a list without
-- replacement.
--
-- >>> pickOne []
-- []
-- >>> pickOne [1]
-- [(1,[])]
-- >>> pickOne [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pickOne :: [a] -> [(a, [a])]
pickOne xs = [(x, l ++ r) | (l, x : r) <- zip (inits xs) (tails xs)]

-- | Iterate through list with index
enumerate :: [b] -> [(Int, b)]
enumerate x = zip [0 .. length x - 1] x

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]