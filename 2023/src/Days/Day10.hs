module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import Debug.Trace (traceShow)
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
parseRow :: (Int, String) -> [(Coords, Char)]
parseRow (y, s) = let
  withXCoords :: [(Int, Char)]
  withXCoords = zip [0..] s
  toCoordsPair (x, c) = ((x, y), c)
  in map toCoordsPair withXCoords

parseRows :: [String] -> Input
parseRows ss = let
  rowIndices = reverse [0..(length ss - 1)]
  rowsWithIndices = zip rowIndices ss
  parsedRows = concat $ map parseRow rowsWithIndices
  in Map.fromList parsedRows

inputParser :: Parser Input
inputParser = do
  rowStrs <- many1 (notChar '\n') `sepBy` endOfLine
  return (parseRows rowStrs)

------------ TYPES ------------
type Coords = (Int, Int)
type Input = Map Coords Char



type OutputA = Int

type OutputB = Int

------------ PART A ------------

north (x, y) = (x, y + 1)
south (x, y) = (x, y - 1)
east (x, y) = (x + 1, y)
west (x, y) = (x - 1, y)

exits :: Input -> Coords -> [Coords]
exits graph coords = let
  exitFuncs = case (Map.findWithDefault '.' coords graph) of
    '.' -> [] -- error ("Why are you navigating from ground coords " ++ show coords)
    '|' -> [north, south]
    '-' -> [east, west]
    'L' -> [north, east]
    'J' -> [north, west]
    '7' -> [south, west]
    'F' -> [south, east]
    'S' -> [north, east, south, west]
  in map (\func -> func coords) exitFuncs

reachableNeighbors :: Input -> Coords -> [Coords]
reachableNeighbors graph coords = let
  candidates = exits graph coords
  reachable n = elem coords (exits graph n)
  in filter reachable candidates

type NeighborFunc v = v -> [(Weight, v)]
type Weight = Int
type DijkstraQueue v = Set (Weight, v)
type DijkstraOutput v = Map v (Weight, Maybe v)

findDeleteMinFilter :: (Ord v) => (v -> Bool) -> Set v -> Maybe (v, Set v)
findDeleteMinFilter f s = findDeleteMinFilter0 f (Set.toList s)

findDeleteMinFilter0 :: (Ord v) => (v -> Bool) -> [v] -> Maybe (v, Set v)
findDeleteMinFilter0 f [] = Nothing
findDeleteMinFilter0 f (x:xs) = if f x then Just (x, Set.fromList xs) else findDeleteMinFilter0 f xs

cacheOrNot :: (Ord v) => v -> DijkstraOutput v -> (Weight, v) -> DijkstraOutput v
cacheOrNot source cache (w, v) = let
  (oldW, _) = Map.findWithDefault (maxBound, undefined) v cache
  in if w < oldW then Map.insert v (w, Just source) cache else cache

dijkstra0 :: (Ord v, Show v) => NeighborFunc v -> DijkstraQueue v -> DijkstraOutput v -> v -> DijkstraOutput v
dijkstra0 neighbors firstQ cache current
  | traceShow (current, Set.size firstQ, Map.size cache) False = undefined
  | isNothing filterResult = cache
  | otherwise = recurse
  where
    (currentWeight, _) = cache!current
    weightedNeighbors = map (\(w, v) -> (w + currentWeight, v)) $ neighbors current
    -- isRelevant :: (Weight, v) -> Bool
    weightFromCache v = fst $ Map.findWithDefault (maxBound, undefined) v cache
    isRelevant (w, v) = w <= weightFromCache v
    beforeFiltering = Set.union firstQ $ Set.fromList weightedNeighbors
    -- filterResult :: Maybe (v, Set v)
    filterResult = findDeleteMinFilter isRelevant beforeFiltering
    ((_, nextNode), nextQ) = fromJust filterResult
    nextCache = foldl (cacheOrNot current) cache weightedNeighbors
    recurse = dijkstra0 neighbors nextQ nextCache nextNode

dijkstra :: (Ord v, Show v) => NeighborFunc v -> v -> DijkstraOutput v
dijkstra f start = dijkstra0 f Set.empty (Map.singleton start (0, Nothing)) start

partA :: Input -> OutputA
partA input = let
  startNode = fst $ head $ filter (\(coords, char) -> char == 'S') $ Map.toList input
  neighborFunc v = zip (repeat 1) $ reachableNeighbors input v
  sssp = dijkstra neighborFunc startNode
  in maximum $ map fst $ Map.elems sssp


------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
