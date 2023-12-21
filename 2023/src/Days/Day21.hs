module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
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

findNeighbors :: Input -> NeighborFunc Coords
findNeighbors graph coords = let
  candidates = [north coords, south coords, east coords, west coords]
  -- this is less efficient than checking the bounds every time but I don't
  -- feel like coding that now
  isValidNeighbor cc = Map.findWithDefault '#' cc graph /= '#'
  in zip (repeat 1) $ filter isValidNeighbor candidates

type NeighborFunc v = v -> [(Weight, v)]
type Weight = Int
type DijkstraQueue v = Set (Weight, v)
type DijkstraOutput v = Map v (Weight, Maybe v)

findDeleteMinFilter :: (Ord v) => (v -> Bool) -> Set v -> Maybe (v, Set v)
findDeleteMinFilter f s = findDeleteMinFilter0 f (Set.toList s)

findDeleteMinFilter0 :: (Ord v) => (v -> Bool) -> [v] -> Maybe (v, Set v)
findDeleteMinFilter0 f [] = Nothing
findDeleteMinFilter0 f (x:xs) = if f x then Just (x, Set.fromList xs) else findDeleteMinFilter0 f xs

dijkstraWithCutoff0 :: (Ord v, Show v) => NeighborFunc v -> Weight -> DijkstraQueue v -> DijkstraOutput v -> v -> DijkstraOutput v
dijkstraWithCutoff0 neighbors maxWeight firstQ cache current
--  | traceShow (current, Set.size firstQ, Map.size cache) False = undefined
  | isNothing filterResult = cache
  | otherwise = recurse
  where
    (currentWeight, _) = cache!current
    weightedNeighbors = map (\(w, v) -> (w + currentWeight, v)) $ neighbors current
    -- isRelevant :: (Weight, v) -> Bool
    weightFromCache v = fst $ Map.findWithDefault (maxBound, undefined) v cache
    isRelevant (w, v) = w <= weightFromCache v && w <= maxWeight
    beforeFiltering = Set.union firstQ $ Set.fromList weightedNeighbors
    -- filterResult :: Maybe (v, Set v)
    filterResult = findDeleteMinFilter isRelevant beforeFiltering
    ((_, nextNode), nextQ) = fromJust filterResult
    addToCache cache0 (ww, w) = Map.insert w (ww, Just current) cache0 
    nextCache = foldl addToCache cache $ filter isRelevant weightedNeighbors
    recurse = dijkstraWithCutoff0 neighbors maxWeight nextQ nextCache nextNode

dijkstraWithCutoff :: (Ord v, Show v) => NeighborFunc v -> Weight -> v -> DijkstraOutput v
dijkstraWithCutoff f maxWeight start = dijkstraWithCutoff0 f maxWeight Set.empty (Map.singleton start (0, Nothing)) start

getStartNode :: Input -> Coords
getStartNode input = let
  filterFunc :: (Coords, Char) -> Bool
  filterFunc (_, char) = char == 'S'
  in fst $ head $ filter filterFunc $ Map.toList input

partA :: Input -> OutputA
partA input = let
  isSample = Map.size input <= 17000
  steps = if isSample then 6 else 64
  start = getStartNode input
  sssp = dijkstraWithCutoff (findNeighbors input) steps start
  evens = filter (\(_, (w, _)) -> w `mod` 2 == 0) $ Map.toList sssp
  in length evens

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
