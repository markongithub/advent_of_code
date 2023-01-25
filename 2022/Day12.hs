module Day12 where

import Data.Char (ord)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

type Label = Char
type Node = (Int, Int)
type Distance = Int
type NodeQueue = Set (Distance, Node)
type Grid = Map Node Label
type Distances = Map Node (Distance, Node)

rowToPairs :: (Int, String) -> [(Node, Label)]
rowToPairs (r, labels) = let
  coords = zip (repeat r) [0..]
  in zip coords labels

rowsToGrid :: [String] -> Grid
rowsToGrid strs = let
  rowIDs = [0..]
  rowPairs :: [(Int, String)]
  rowPairs = zip rowIDs strs
  nodesByRow :: [[(Node, Label)]]
  nodesByRow = map rowToPairs rowPairs
  in Map.fromList $ concat nodesByRow

isEdge :: Grid -> Node -> Node -> Bool
isEdge grid n1 n2 = let
  label1 = grid!n1 -- don't set n1 to an invalid value
  in case Map.lookup n2 grid of
    Nothing -> False
    Just label2 -> elevation label2 - elevation label1 <= 1

elevation :: Char -> Int
elevation 'E' = ord 'z'
elevation 'S' = ord 'a'
elevation other = ord other

findNeighbors :: Grid -> Node -> [Node]
findNeighbors g (x,y) = let
  candidates = [(x,y-1), (x-1,y), (x,y+1), (x+1,y)]
  in filter (isEdge g (x, y)) candidates

updateDistance :: (Distances, NodeQueue) -> (Node, Distance, Node) -> (Distances, NodeQueue)
updateDistance (ds, q) (n, dist, prev) = let
  better = Map.notMember n ds || dist < (fst $ lookupOrError n ds "updateDistance")
  in case better of
    True -> (Map.insert n (dist, prev) ds, Set.insert (dist, n) q)
    False -> (ds, q)

lookupOrError :: (Ord k, Show k) => k -> Map k a -> String -> a
lookupOrError k m errorStr = case Map.lookup k m of
  Nothing -> error (errorStr ++ " (key was " ++ show k ++ ")")
  Just a -> a

dijkstra0 :: Grid -> Distances -> Set Node -> Set (Int, Node) -> Node -> Node -> Maybe Distances
dijkstra0 grid distances visited queue current destination = let
  currentDistance = fst $ lookupOrError current distances "distances!current"
  neighbors = findNeighbors grid current
  neighborDistances = repeat (currentDistance + 1) -- I bet we change this in part 2
  neighborsWithDists = zip3 neighbors neighborDistances (repeat current)
  (newDistances, newQueue) = if (null neighborsWithDists) then error "neighborsWithDists is empty" else foldl updateDistance (distances, queue) neighborsWithDists
  newVisited = Set.insert current visited
  newCurrent = snd $ Set.findMin newQueue
  finalQueue = Set.deleteMin newQueue
  recurse = dijkstra0 grid newDistances newVisited finalQueue newCurrent destination
  in if current == destination
        then Just distances
        else if Set.null newQueue
               then Nothing
               else recurse

dijkstra :: Grid -> Node -> Node -> Maybe Distances
dijkstra gridD source destination = let
  initialDistances = Map.singleton source (0, undefined)
  initialVisited = Set.empty
  initialQueue = Set.empty
  in dijkstra0 gridD initialDistances initialVisited initialQueue source destination

traceBack :: Distances -> Node -> Node -> [Node] -> [Node]
traceBack ds source destination accu = let
  (_, next) = lookupOrError destination ds "ds!destination"
  newAccu = next:accu
  in if source == destination then accu else traceBack ds source next newAccu

shortestPath :: Grid -> Node -> Node -> Maybe ([Node], Distance)
shortestPath g source destination = let
  dijkstraOutput = dijkstra g source destination
  fullLength paths = fst $ lookupOrError destination paths "paths!destination"
  path paths = traceBack paths source destination []
  in case dijkstraOutput of
    Just paths -> Just (path paths, fullLength paths)
    Nothing -> Nothing

-- stupid and inefficient but still O(n)
findStartAndEnd :: Grid -> (Node, Node)
findStartAndEnd grid = let
  pairs = Map.toList grid
  start = fst $ head $ filter (\p -> snd p == 'S') pairs
  end = fst $ head $ filter (\p -> snd p == 'E') pairs
  in (start, end)

solvePart1Pure :: [String] -> Int
solvePart1Pure strings = let
  grid = rowsToGrid strings
  (start, end) = findStartAndEnd grid
  in snd $ fromJust $ shortestPath grid start end

solvePart1 = do
  text <- readFile "data/input12.txt"
  (return . solvePart1Pure . lines) text

allLowElevations :: Grid -> [Node]
allLowElevations grid = let
   pairs = Map.toList grid
   lowPairs = filter (\p -> snd p == 'a' || snd p == 'S') pairs
   in map fst lowPairs

solvePart2Pure :: [String] -> Int
solvePart2Pure strings = let
  grid = rowsToGrid strings
  (_, end) = findStartAndEnd grid
  starts = allLowElevations grid
  runSP start = shortestPath grid start end
  lengths = map snd $ catMaybes $ map runSP starts
  in minimum lengths

solvePart2 = do
  text <- readFile "data/input12.txt"
  (return . solvePart2Pure . lines) text

testInput = [
    "Sabqponm"
  , "abcryxxl"
  , "accszExk"
  , "acctuvwj"
  , "abdefghi"
  ]