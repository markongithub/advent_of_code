module Day24 where

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Color = White | Black deriving (Eq, Show)

type Location = (Int, Int)
type Tiles = Map Location Color

addPairs :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
addPairs (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sumPairs :: (Num a, Num b) => [(a,b)] -> (a,b)
sumPairs xs = foldl addPairs (0,0) xs

parseDirections :: String -> [Location]
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

flipTile :: Tiles -> Location -> Tiles
flipTile m k = let
  newColor = case Map.lookup k m of
    Nothing -> Black
    Just c -> flipColor c
  in Map.insert k newColor m

-- I can't use Map.union for this because the order of updating matters
flipTiles :: Tiles -> [Location] -> Tiles
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

adjacentOffsets :: [Location]
adjacentOffsets = map (head . parseDirections) ["e", "se", "sw", "w", "nw", "ne"]

adjacentsOfTile :: Location -> [Location]
adjacentsOfTile tile = map (addPairs tile) adjacentOffsets

addImplicitAdjacents :: Tiles -> Location -> Tiles
addImplicitAdjacents m location = let
  adjacents = adjacentsOfTile location
  makePair t = (t, Map.findWithDefault White t m)
  in Map.union m (Map.fromList $ map makePair adjacents)

addAllImplicitAdjacents :: Tiles -> Tiles
addAllImplicitAdjacents m = let
  blackLocations = map fst $ filter (\p -> snd p == Black) $ Map.toList m
  in foldl addImplicitAdjacents m blackLocations

countBlackAdjacents :: Tiles -> Location -> Int
countBlackAdjacents m t = let
  adjacents = adjacentsOfTile t
  colors = map (\t -> Map.findWithDefault White t m) adjacents
  in length $ filter (== Black) colors

newColor :: Tiles -> (Location, Color) -> Color
newColor m (location, color) = let
  blackAdjacents = countBlackAdjacents m location
  in case (color, blackAdjacents) of
    (Black, 0) -> White
    (Black, 1) -> Black
    (Black, 2) -> Black
    (Black, _) -> White
    (White, 2) -> Black
    (White, _) -> White

flipColors :: Tiles -> Tiles
flipColors m = let
  newPair (l, c) = (l, newColor m (l, c))
--  in Map.fromList $ map newPair $ Map.toList $ addAllImplicitAdjacents m
  in Map.fromAscList $ filter (\(l, c) -> c == Black) $ map newPair $ Map.toAscList $ addAllImplicitAdjacents m

blackAfterNDays :: Tiles -> Int -> Int
blackAfterNDays m n = countBlackTiles $ (iterate flipColors m)!!n

solvePart2Func :: Int -> [String] -> Int
solvePart2Func d input = let
  tiles = map (sumPairs . parseDirections) input
  part1Map = flipTiles Map.empty tiles
  in blackAfterNDays part1Map d

solvePart2 :: IO Int
solvePart2 = let
  input = readFile "data/input24.txt"
  in fmap ((solvePart2Func 100) . lines) input
