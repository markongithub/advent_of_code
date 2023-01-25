module Day18 where

import Common (splitAtSep)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

testInputOld = [
    "2,2,2"
  , "1,2,2"
  , "3,2,2"
  , "2,1,2"
  , "2,3,2"
  , "2,2,1"
  , "2,2,3"
  , "2,2,4"
  , "2,2,6"
  , "1,2,5"
  , "3,2,5"
  , "2,1,5"
  , "2,3,5"
  ]

testInput = [
    "1,1,1"
  , "2,1,1"
  , "3,1,1"
  , "4,1,1"
  , "5,1,1"
  , "6,1,1"
  , "1,2,1"
  , "2,2,1"
  , "3,2,1"
  , "4,2,1"
  , "5,2,1"
  , "6,2,1"
  , "1,3,1"
  , "2,3,1"
  , "3,3,1"
  , "4,3,1"
  , "5,3,1"
  , "6,3,1"
  , "1,1,2"
  , "2,1,2"
  , "3,1,2"
  , "4,1,2"
  , "5,1,2"
  , "6,1,2"
  , "1,2,2"
  , "6,2,2"
  , "1,3,2"
  , "2,3,2"
  , "3,3,2"
  , "4,3,2"
  , "5,3,2"
  , "6,3,2"
  , "1,1,3"
  , "2,1,3"
  , "3,1,3"
  , "4,1,3"
  , "5,1,3"
  , "6,1,3"
  , "1,2,3"
  , "2,2,3"
  , "3,2,3"
  , "4,2,3"
  , "5,2,3"
  , "6,2,3"
  , "1,3,3"
  , "2,3,3"
  , "3,3,3"
  , "4,3,3"
  , "5,3,3"
  , "6,3,3"
  ]

parseLine :: String -> (Int, Int, Int)
parseLine s = let
  [s1, s2, s3] = splitAtSep s ','
  in (read s1 :: Int, read s2 :: Int, read s3 :: Int)

type TwoDToThird = Map (Int, Int) (Set Int)
type TripleSplitter = (Int, Int, Int) -> ((Int, Int), Int)

groupByTwo :: [(Int, Int, Int)] -> TripleSplitter -> TwoDToThird
groupByTwo triples func = let
  pairs = map func triples
  insertPair :: TwoDToThird -> ((Int, Int), Int) -> TwoDToThird
  insertPair oldMap (pair, z) = Map.insertWith Set.union pair (Set.singleton z) oldMap
  in foldl insertPair Map.empty pairs

consecutives0 :: [Int] -> Int -> Int
consecutives0 [] accu = accu
consecutives0 [x] accu = accu
consecutives0 (x:(y:ys)) accu = let
  newAccu = if (y == x + 1) then accu + 2 else accu
  in consecutives0 (y:ys) newAccu

consecutives :: [Int] -> Int
consecutives ls = consecutives0 ls 0

coveredSides :: TwoDToThird -> Int
coveredSides imap = let
  lists :: [[Int]]
  lists = map (Set.toList) $ Map.elems imap
  in sum $ map consecutives lists

coveredSides3D :: [(Int, Int, Int)] -> Int
coveredSides3D triples = let
  xFunc (x, y, z) = ((y, z), x)
  yFunc (x, y, z) = ((x, z), y)
  zFunc (x, y, z) = ((x, y), z)
  [xMap, yMap, zMap] = map (groupByTwo triples) [xFunc, yFunc, zFunc]
  in sum $ map coveredSides [xMap, yMap, zMap]

surfaceArea :: [(Int, Int, Int)] -> Int
surfaceArea triples = let
  totalArea = 6 * length triples
  coveredArea = coveredSides3D triples
  in totalArea - coveredArea

testTriples = map parseLine testInput

solvePart1 :: IO Int
solvePart1 = let
  text = readFile "data/input18.txt"
  triples = fmap (map parseLine . lines) text
  in fmap surfaceArea triples

type Cell = (Int, Int, Int)
minMaxXYZ :: [(Int, Int, Int)] -> ((Int, Int, Int), (Int, Int, Int))
minMaxXYZ triples = let
  xs = map (\(q, _, _) -> q) triples
  ys = map (\(_, q, _) -> q) triples
  zs = map (\(_, _, q) -> q) triples
  in ((minimum xs, minimum ys, minimum zs), (maximum xs, maximum ys, maximum zs))

minCoord = -1
maxCoord = 22

cellsNextToDroplet :: Cell -> [Cell]
cellsNextToDroplet (x, y, z) = [
    (min maxCoord (x + 1),     y,     z)
  , (max minCoord (x - 1),     y,     z)
  , (    x, min maxCoord (y + 1),     z)
  , (    x, max minCoord (y - 1),     z)
  , (    x,     y, min maxCoord (z + 1))
  , (    x,     y, max minCoord (z - 1))
  ]

allEmptyBorderCells :: [Cell] -> Set Cell
allEmptyBorderCells cells = let
  borderCells = Set.unions $ map (Set.fromList . cellsNextToDroplet) cells
  dropletCells = Set.fromList cells
  in Set.difference borderCells dropletCells

outsideCell :: Cell
outsideCell = (10, 10, 22)

myCrappySearch :: Set Cell -> Set Cell -> Set Cell -> Cell -> Set Cell
myCrappySearch droplets explored toExplore current = let
  neighbors = Set.fromList $ cellsNextToDroplet current
  emptyNeighbors = Set.difference neighbors droplets
  unexploredEmptyNeighbors = Set.difference emptyNeighbors explored
  newToExplore1 = Set.delete current toExplore
  newToExplore2 = Set.union newToExplore1 unexploredEmptyNeighbors
  (newCurrent, newToExplore3) = Set.deleteFindMin newToExplore2
  newExplored = Set.insert current explored
  recurse = myCrappySearch droplets newExplored newToExplore3 newCurrent
  in case Set.null newToExplore3 of
    True -> newExplored
    False -> recurse

allReachableFromOutside :: [Cell] -> Set Cell
allReachableFromOutside cells = myCrappySearch (Set.fromList cells) Set.empty Set.empty outsideCell

findUnreachableCells :: [Cell] -> Set Cell
findUnreachableCells cells = let
  reachableFromOutside = allReachableFromOutside cells
  in Set.difference (allEmptyBorderCells cells) reachableFromOutside

reachableSides :: Set Cell -> Cell -> Set Cell
reachableSides reachable droplet = let
  neighbors :: Set Cell
  neighbors = Set.fromList $ cellsNextToDroplet droplet
  in Set.intersection reachable neighbors

solvePart2Pure :: [Cell] -> Int
solvePart2Pure cells = let
  reachable = allReachableFromOutside cells
  sidesPerCell = map (Set.size . reachableSides reachable) cells
  in sum sidesPerCell

solvePart2 :: IO Int
solvePart2 = let
  text = readFile "data/input18.txt"
  triples = fmap (map parseLine . lines) text
  in fmap solvePart2Pure triples

puzzleInput :: IO [Cell]
puzzleInput = let
  text = readFile "data/input18.txt"
  in fmap (map parseLine . lines) text
