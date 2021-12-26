module Day25.Mod where

-- import Data.Map (Map as M)
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import GHC.Arr (Array, array, assocs, bounds, eqArray, (!), (//))
import Utils.Coords
import Utils.Mod

data Tile = East | South | Empty deriving (Show, Eq, Ord)

toTile :: Char -> Tile
toTile '.' = Empty
toTile '>' = East
toTile 'v' = South

tileToChar :: Tile -> Char
tileToChar Empty = '.'
tileToChar East = '>'
tileToChar South = 'v'

type Map = Array Coord Tile

-- Get the coord mod the bounds so its always in the map
wrapCoord :: Map -> Coord -> Coord
wrapCoord m (C row col) = C (row `wrap` hirow) (col `wrap` hicol)
  where
    (_, C hirow hicol) = bounds m
    wrap x bound = if x > bound then 0 else x

getTile :: Map -> Coord -> Tile
getTile m c = m ! wrapCoord m c

getMap :: [String] -> Map
getMap s = array (low, high) tiles
  where
    tiles = coordLines $ map (map toTile) s
    Just (low, high) = boundingBox (map fst tiles)

getTilesOfType :: Map -> Tile -> [(Coord, Tile)]
getTilesOfType m t = filter (\(_, tile) -> tile == t) (assocs m)

tileToDirFunc :: Tile -> (Coord -> Coord)
tileToDirFunc Empty = error "no func"
tileToDirFunc East = right
tileToDirFunc South = below

getMove :: Map -> Coord -> Maybe [(Coord, Tile)]
getMove m c
  | tile == Empty = Nothing
  | otherwise = if getTile m newCoord == Empty then Just [(wrapCoord m newCoord, tile), (c, Empty)] else Nothing
  where
    tile = getTile m c
    dirFunc = tileToDirFunc tile
    newCoord = dirFunc c

-- Check and apply moves for east, then check and apply moves for south
getMoves :: Map -> Tile -> [(Coord, Tile)]
getMoves m t = concat moves
  where
    tiles = getTilesOfType m t
    moves = mapMaybe (getMove m . fst) tiles

-- Given a list of coords to change apply them
applyMoves :: Map -> [(Coord, Tile)] -> Map
applyMoves m updates = m // updates

getAndApply :: Map -> Tile -> Map
getAndApply m t = applyMoves m (getMoves m t)

runStep :: Map -> Map
runStep m = getAndApply eastStep South
  where
    eastStep = getAndApply m East

runTillStop :: Int -> Map -> Int
runTillStop n m = if eqArray m step then n + 1 else runTillStop (n + 1) step
  where
    step = runStep m

readMap :: IO Map
readMap = getMap <$> readInputLines

drawMap :: Map -> String
drawMap m = drawCoordsGen tileToChar Empty (M.fromList $ assocs m)

runNSteps :: Int -> Map -> Map
runNSteps n m = iterate runStep m !! n

runAndDraw :: Int -> Map -> String
runAndDraw n m = drawMap (runNSteps n m)

part1 :: IO ()
part1 = do
  input <- readMap
  -- putStrLn $ runAndDraw 1 input
  -- putStrLn $ drawMap input
  print $ runTillStop 0 input
  return ()

part2 :: IO ()
part2 = do
  input <- readMap
  print "part2"
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]