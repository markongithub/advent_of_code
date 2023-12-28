module Days.Day21 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
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

findNeighborsInfinite :: Input -> Coords -> NeighborFunc Coords
findNeighborsInfinite graph (width, height) coords = let
  fixCoords (x0, y0) = (x0 `mod` width, y0 `mod` height)
  candidates = [north coords, south coords, east coords, west coords]
  isValidNeighbor cc = case Map.lookup (fixCoords cc) graph of
    Just c -> c /= '#'
    Nothing -> error ("why are we looking up " ++ (show (cc, fixCoords cc)))
  in zip (repeat 1) $ filter isValidNeighbor candidates


makePartBSSSP :: Input -> Weight -> DijkstraOutput Coords
makePartBSSSP input steps = let
  start = getStartNode input
  width = 1 + (maximum $ map fst $ Map.keys input)
  height = 1 + (maximum $ map snd $ Map.keys input)
  sssp = dijkstraWithCutoff (findNeighborsInfinite input (width, height)) steps start
  in sssp

addDistance :: (Weight, Weight) -> Int -> Coords ->  Coords
addDistance (width, height) multiple (x, y) = (x + (multiple * width), y + (multiple * height))

lookupWithError :: (Ord k, Show k) => k -> Map k a -> a
lookupWithError k m = case Map.lookup k m of
  Just result -> result
  Nothing -> error ("missing key: " ++ show k)

totalEvensOdds :: DijkstraOutput Coords -> (Int,Int,Int)
totalEvensOdds sssp = let
  evens = Map.filter (\p -> fst p `mod` 2 == 0) sssp
  odds = Map.filter (\p -> fst p `mod` 2 == 1) sssp
  in (Map.size sssp, Map.size evens, Map.size odds)

parseFile :: String -> IO Input
parseFile filename = do
  contents <- readFile filename
  case (parseOnly inputParser $ pack contents) of
    Left e -> error e
    Right i -> return i

-- partB :: Input -> OutputB
partB input = let
  start = getStartNode input
  width = 1 + (maximum $ map fst $ Map.keys input)
  height = 1 + (maximum $ map snd $ Map.keys input)
  exploreBaseMap = dijkstraWithCutoff (findNeighbors input) maxBound start
  reachableNodes = Map.keys exploreBaseMap
  horizontalScreensAway = 2
  nodesToCheck = map (addDistance (width, 0) horizontalScreensAway) reachableNodes
  infiniteSSSP = makePartBSSSP input 43
  evens = Map.filter (\p -> fst p `mod` 2 == 0) infiniteSSSP
  odds = Map.filter (\p -> fst p `mod` 2 == 1) infiniteSSSP
  -- in (Map.size infiniteSSSP, Map.size evens, Map.size odds)
  -- results = filter (\p -> fst p == 14) $ Map.elems exploreBaseMap
  results = map (\n -> fst (lookupWithError n infiniteSSSP)) nodesToCheck
  in (minimum results, maximum results)
-- it takes 0-14 to explore the base map
--
-- (22,40) to go 2 horizontal screens away
-- (33,51) to go 3 horizontal screens away
-- it takes 12,32 to go 1 diagonal screen away
-- it takes 34,54 to go 2 diagonal screens away
-- it takes 56,76 to go 3 diagonal screens away
-- WH = width + height
-- in each block of the sample input I can reach 81 squares - 42 with an even number
-- of steps, 39 with an odd number
-- for every 11 steps I add, I increase my reach by 81/42/39 IN EVERY DIRECTION
-- 1^2 -> 3^2 -> 5^2 no this is wrong
{-
x
      x
then xxx
      x
       
       x
      xxx
then xxxxx
      xxx
       x
1 then 1+3+1 then 1+3+5+3+1 then 1+3+5+7+5+3+1
                  3+5            7 + 5
1          4        8              12
    Sum of 4 
    12 24 40
    3 6 10 
-- 18 steps to fill the 131
-- after 22 you BEGIN the 13531 which has 13 squares

-- 40 steps to form the 13531 = 13*(81/42/39) = 1 + 4*3 = 1 + 4 * ((2^2 + 2) / 2)
-- 33 to begin the 1357531
-- 51 steps to finish the 1357531 = 25*(81/42/39) = 1 + 4 * ((3^2 + 3) /2)
-- 44 you will begin the 
-- = 18 + 
-- 18 + (4*11) steps to fill 1 + 4 * ((4*4 + 4) / 2)
-- 18 + (452*11) = 4990 = 1 + 4 * (452 * 453) /2 = 1 + 2*452*453
-- 
-- to explore the initial 
-- in 32 steps I can go (1179,605,574)
-- in 43 steps I can go (2253,1107,1146)
-- in 54 steps I can go (3658,1853,1805)
-}
