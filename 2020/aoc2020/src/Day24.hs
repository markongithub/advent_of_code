module Day24 where

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Color = White | Black deriving (Eq, Show)

type Tiles = Map (Int, Int) Color

addPairs :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sumPairs :: (Num a, Num b) => [(a,b)] -> (a,b)
sumPairs xs = foldl addPairs (0,0) xs

parseDirections :: String -> [(Int, Int)]
parseDirections [] = []
parseDirections (x:xs) = let
  parseXS = parseDirections xs
  parseTailXS = parseDirections (tail xs)
  getX y = case (head xs) of
    'e' -> ( 1, y):parseTailXS
    'w' -> (-1, y):parseTailXS
    _   -> error "didn't get ew after ns"
  in case x of
    'w' -> (-2,0):parseXS
    'e' -> (2, 0):parseXS
    's' -> getX (-1)
    'n' -> getX 1
    _   -> error "got something other than nesw"

flipColor :: Color -> Color
flipColor Black = White
flipColor White = Black

flipTile :: Tiles -> (Int, Int) -> Tiles
flipTile m k = let
  newColor = case Map.lookup k m of
    Nothing -> Black
    Just c -> flipColor c
  in Map.insert k newColor m

-- I can't use Map.union for this because the order of updating matters
flipTiles :: Tiles -> [(Int, Int)] -> Tiles
flipTiles m tiles = foldl flipTile m tiles

countBlackTiles :: Tiles -> Int
countBlackTiles m = length $ filter (== Black) $ Map.elems m

solvePart1Func :: [String] -> Int
solvePart1Func input = let
  tiles = map (sumPairs . parseDirections) input
  in countBlackTiles $ flipTiles Map.empty tiles

solvePart1 :: IO Int
solvePart1 = let
  input = readFile "data/input24.txt"
  in fmap (solvePart1Func . lines) input
