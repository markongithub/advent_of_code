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
--  | traceShow (current, Set.size firstQ, Map.size cache) False = undefined
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

findLoop :: Input -> DijkstraOutput Coords
findLoop input = let
  startNode = fst $ head $ filter (\(coords, char) -> char == 'S') $ Map.toList input
  neighborFunc v = zip (repeat 1) $ reachableNeighbors input v
  in dijkstra neighborFunc startNode

partA :: Input -> OutputA
partA input = let
  sssp = findLoop input
  in maximum $ map fst $ Map.elems sssp

------------ PART B ------------
onPerimeter :: (Int, Int, Int, Int) -> Coords -> Bool
onPerimeter (minX, maxX, minY, maxY) (x, y) =
  x == minX || x == maxX || y == minY || y == maxY

nearbyGround :: Input -> NeighborFunc Coords
nearbyGround graph (x,y) = let
  -- candidates = map (\func -> func coords) [north, south, east, west]
  candidates = [(x,y) | x <- [x-1,x,x+1], y<- [y-1, y, y+1]]
  isGround c = Map.lookup c graph == Just '.'
  in zip (repeat 1) $ filter isGround candidates

getContiguousGround :: Input -> Coords -> [Coords]
getContiguousGround graph coords = let
  sssp = dijkstra (nearbyGround graph) coords
  in Map.keys sssp

-- for every square in the loop, get its ground neighbors
-- maintain a set of ground squares that touch the perimeter
-- for every ground square touching the loop

allGroundNearLoop :: Input -> [Coords] -> [Coords]
allGroundNearLoop input loop = let
  iHateThis1 :: Coords -> [(Weight, Coords)]
  iHateThis1 = nearbyGround input
  listOfLists :: [[(Weight, Coords)]]
  listOfLists = map iHateThis1 loop
  listOfListsOfCoords = map (map snd) listOfLists 
  in Set.toList $ Set.fromList $ concat listOfListsOfCoords

{-
FJL7L7LJLJ||LJI
01234567890123
 FJ L7 L7 LJ LJ | | LJ I
01  2  3  4  5  6 7 8
 FJ L7 L7 LJ LJ | | LJ I
o  i  o  i  o  i o i  o
-}

rayCastInLoop :: Input -> Set Coords -> Coords -> Bool
rayCastInLoop graph loop (x, y) = let
  westernRay = zip [0..(x-1)] (repeat y)
  toSubtract = min x y
  xRange = [(x - toSubtract)..(x - 1)]
  yRange = [(y - toSubtract)..(y - 1)]
  ray0 = zip xRange yRange
  ray = traceShow ("ray coordinates: " ++ show ray0) ray0
  transitions = countTransitionsIntoLoop graph loop ray
  in transitions `mod` 2 == 1

countTransitionsIntoLoop :: Input -> Set Coords -> [Coords] -> Int
countTransitionsIntoLoop graph loop xs = countTransitionsIntoLoop0 graph loop xs 0

countTransitionsIntoLoop0 :: Input -> Set Coords -> [Coords] -> Int -> Int
countTransitionsIntoLoop0 graph loop [] accu = accu
countTransitionsIntoLoop0 graph loop (x:xs) accu = let
  matters = elem (graph!x) ['7', 'L', '|', '-']
  -- O - L O
  isTransition = Set.member x loop && matters
  newAccu = if isTransition then accu + 1 else accu
  in countTransitionsIntoLoop0 graph loop xs newAccu

countTransitionsIntoPipe :: Input -> Set Coords -> [Coords] -> Int
countTransitionsIntoPipe graph loop xs = countTransitionsIntoPipe0 graph loop xs False 0

countTransitionsIntoPipe0 :: Input -> Set Coords -> [Coords] -> Bool -> Int -> Int
countTransitionsIntoPipe0 graph loop [] _ accu = accu
countTransitionsIntoPipe0 graph loop (x:xs) wasInPipe accu = let
  nowInPipe = Set.member x loop
  isTransition = nowInPipe && not wasInPipe
  ongoingPipe = elem (graph!x) ['-', 'L', 'F']
  newAccu = if isTransition then accu + 1 else accu
  in countTransitionsIntoPipe0 graph loop xs ongoingPipe newAccu
   
countTransitionsToTrue :: [Bool] -> Int
countTransitionsToTrue xs = countTransitionsToTrue0 xs False 0

countTransitionsToTrue0 :: [Bool] -> Bool -> Int -> Int
countTransitionsToTrue0 [] _ accu = accu
countTransitionsToTrue0 (x:xs) last accu = let
  isTransition = x && not last
  newAccu = if isTransition then accu + 1 else accu
  in countTransitionsToTrue0 xs x newAccu

findInnerGround :: Input -> (Int, Int, Int, Int) -> Set Coords -> [Coords] -> Set Coords ->  Set Coords -> ([Coords], [Coords])
findInnerGround _ _ _ [] outer inner = (Set.toList inner, Set.toList outer)
findInnerGround graph bounds loop (x:xs) outer inner = let
  alreadyExploredX = Set.member x outer || Set.member x inner
  justRecurse = findInnerGround graph bounds loop xs outer inner
  xIsGround = graph!x == '.'
  groundFromX = getContiguousGround graph x
  connectedToPerimeter = traceShow ("getContiguousGround " ++ show x ++ " returns " ++ show groundFromX) $ any (onPerimeter bounds) groundFromX
  isOuter0 = not $ rayCastInLoop graph loop x
  isOuter = traceShow ("rayCastInLoop " ++ show x ++ " returns " ++ show (not isOuter0)) isOuter0
  newOuter = if isOuter then Set.union outer (Set.fromList groundFromX) else outer
  newInner = if isOuter then inner else Set.union inner (Set.fromList groundFromX)
  in if alreadyExploredX then justRecurse else findInnerGround graph bounds loop xs newOuter newInner

findBounds :: Input -> (Int, Int, Int, Int)
findBounds input = let
  allCoords = Map.keys input
  (allX, allY) = unzip allCoords
  in (minimum allX, maximum allX, minimum allY, maximum allY)

neighborsNotInLoop :: Input -> Set Coords -> NeighborFunc Coords
neighborsNotInLoop graph loop (x, y) = let
  allNeighbors = [(nX, nY) | nX <- [x-1,x,x+1], nY <- [y-1, y, y+1]]
  validCoords n = Map.member n graph
  notInLoop n = not $ Set.member n loop
  filterFunc n = notInLoop n && validCoords n
  in zip (repeat 1) $ filter filterFunc allNeighbors

exploreOutsideLoop :: Input -> Set Coords -> Coords -> [Coords]
exploreOutsideLoop graph loop coords = let
  sssp = dijkstra (neighborsNotInLoop graph loop) coords
  in Map.keys sssp

findInnerTiles :: Input -> (Int, Int, Int, Int) -> Set Coords -> [Coords] -> Set Coords ->  Set Coords -> ([Coords], [Coords])
findInnerTiles _ _ _ [] outer inner = (Set.toList inner, Set.toList outer)
findInnerTiles graph bounds loop (x:xs) outer inner = let
  alreadyExploredX = Set.member x outer || Set.member x inner
  justRecurse = findInnerTiles graph bounds loop xs outer inner
  contiguousFromX = exploreOutsideLoop graph loop x
  connectedToPerimeter0 = any (onPerimeter bounds) contiguousFromX
  connectedToPerimeter = traceShow ("Are these connected to the perimeter? " ++ show (connectedToPerimeter0, contiguousFromX)) connectedToPerimeter0 
  isOuterRayCast0 = not $ rayCastInLoop graph loop x
  isOuterRayCast = traceShow ("Does ray cast think this is outer? " ++ show (isOuterRayCast0, x)) isOuterRayCast0
  isOuter = connectedToPerimeter || isOuterRayCast
--  isOuter = traceShow ("rayCastInLoop " ++ show x ++ " returns " ++ show (not isOuter0)) isOuter0
  newOuter = if isOuter then Set.union outer (Set.fromList contiguousFromX) else outer
  newInner = if isOuter then inner else Set.union inner (Set.fromList contiguousFromX)
  in if alreadyExploredX then justRecurse else findInnerTiles graph bounds loop xs newOuter newInner

fixStartNode :: Input -> Input
fixStartNode input = let
  startNode = fst $ head $ filter (\(coords, char) -> char == 'S') $ Map.toList input
  neighbors = Set.fromList $ reachableNeighbors input startNode
  candidates = "|-LJ7F"
  fixGraph :: Char -> Input
  fixGraph c = Map.insert startNode c input
  allFixes = map fixGraph candidates
  checkSets = zip allFixes $ map (\g -> Set.fromList $ reachableNeighbors g startNode) allFixes
  validFixes :: [(Input, Set Coords)]
  validFixes = filter (\(f, n) -> n == neighbors) checkSets
  output = fst $ head validFixes
  debugMessage = "The S pipe is really a " ++ show (output!startNode)
  in case length validFixes of
    1 -> traceShow debugMessage output
    x -> error ("I got non-one valid fixes: " ++ show validFixes)

partB :: Input -> (OutputB, [Coords], [Coords])
partB input0 = let
  input = fixStartNode input0
  loop = Map.keys $ findLoop input0
  loopSet = traceShow ("loop: " ++ show loop) $ Set.fromList loop
  bounds0 = findBounds input
  bounds = traceShow ("bounds: " ++ show bounds0) bounds0
  allTiles = filter (\t -> not $ Set.member t loopSet) $ Map.keys input
  (innerTiles, outerTiles) = findInnerTiles input bounds loopSet allTiles Set.empty Set.empty
  in (length innerTiles, innerTiles, outerTiles)