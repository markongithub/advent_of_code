module Day24 where

import Data.List (partition, sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

import AStar (shortestPathAStar)
import BreadthFirstSearch (shortestPathBFS)

type Coords = (Int, Int)

data Direction = North | South | West | East
                 deriving (Eq, Ord, Show)

type Blizzard = (Coords, Direction)
type BlizzardMap = Set Blizzard

moveDirection :: Coords -> Coords -> Blizzard -> Coords
moveDirection (_, bottomWall) (_, topWall) ((x, y), North) = (x, if y + 1 == topWall then bottomWall + 1 else y + 1)
moveDirection (_, bottomWall) (_, topWall) ((x, y), South) = (x, if y - 1 == bottomWall then topWall - 1 else y - 1)
moveDirection (leftWall, _) (rightWall, _) ((x, y), East) = (if x + 1 == rightWall then leftWall + 1 else x + 1, y)
moveDirection (leftWall, _) (rightWall, _) ((x, y), West) = (if x - 1 == leftWall then rightWall - 1 else x - 1, y)

moveAllBlizzards :: Coords -> Coords -> BlizzardMap -> BlizzardMap
moveAllBlizzards bottomLeft topRight bMap = let
  aToA :: Blizzard -> Blizzard
  aToA (coords, dir) = (moveDirection bottomLeft topRight (coords, dir), dir)
  in Set.map aToA bMap

parseChar :: Char -> Maybe Direction
parseChar 'v' = Just South
parseChar '<' = Just West
parseChar '^' = Just North
parseChar '>' = Just East
parseChar _ = Nothing

parseIntermediateLine :: (Int, String) -> BlizzardMap
parseIntermediateLine (row, str) = let
  filterSndJust [] = []
  filterSndJust ((c, Nothing):xs) = filterSndJust xs
  filterSndJust ((c, Just d):xs) = (c, d):(filterSndJust xs)
  directionsByColumn :: [(Int, Direction)]
  directionsByColumn = filterSndJust $ zip [0..] $ map parseChar str
  directionsByCoords = map (\(c, d) -> ((c, row), d)) directionsByColumn
  in Set.fromList directionsByCoords

parseGrid :: [String] -> (Coords, Coords, BlizzardMap)
parseGrid strs = let
  bottomLeft = (0,0)
  topRight = (length (head strs) - 1, length strs - 1)
  intermediateLines = init $ tail $ zip (reverse [0..(length strs - 1)]) strs
  allBlizzards = Set.unions $ map parseIntermediateLine intermediateLines
  in (bottomLeft, topRight, allBlizzards)

testInput = [
    "#.#####"
  , "#.....#"
  , "#>....#"
  , "#.....#"
  , "#...v.#"
  , "#.....#"
  , "#####.#"
  ]

moveNTurns :: Int -> Coords -> Coords -> BlizzardMap -> BlizzardMap
moveNTurns 0 _ _ bMap = bMap
moveNTurns turns bl tr bMap = let
  nextMap = moveAllBlizzards bl tr bMap
  in moveNTurns (turns - 1) bl tr nextMap

isGoal :: Coords -> Coords -> Coords -> Bool
isGoal (_, minY) (maxX, _) (x, y) = x == maxX - 1 && y == minY

availableMoves :: BlizzardMap2 -> Node -> [Node]
availableMoves bMap ((x, y), turn) = let
    BlizzardMap2 (minX, minY) (maxX, maxY) _ _ = bMap
    allMoves :: [Coords]
    allMoves = [ (x + 1, y), (x, (y - 1)), (x - 1, y), (x, y),
                 (x, (y + 1))]
    unoccupied :: Coords -> Bool
    unoccupied (q, r) = not $ hasBlizzard bMap (q,r) (turn + 1)
    emptySpaces1 :: [Coords]
    emptySpaces1 = filter unoccupied allMoves
    inBounds :: Coords -> Bool
    inBounds (x, y) = x > minX && x < maxX && y > minY && y < maxY
    allowed :: Coords -> Bool
    allowed (x, y) = (x,y) == fst (startNode bMap) || isGoal (minX, minY) (maxX, maxY) (x, y) || inBounds (x, y)
    emptySpaces2 = filter allowed emptySpaces1
    in zip emptySpaces2 $ repeat (turn + 1)

type Node = (Coords, Int)
type Distance = Int
type NodeQueue = Set (Distance, Node)
type Distances = Map Node (Distance, Node)
type TargetFunc = Node -> Bool

findNeighbors :: BlizzardMap2 -> Node -> [Node]
findNeighbors = availableMoves

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

dijkstra0 :: BlizzardMap2 -> Distances -> Set Node -> Set (Int, Node) -> Node -> TargetFunc -> Maybe Distances
dijkstra0 initialMap distances visited queue current destination = let
  currentDistance = fst $ lookupOrError current distances "distances!current"
  neighborsD = findNeighbors initialMap current
  neighbors = neighborsD -- if null neighborsD then error ("no neighbors from " ++ show current) else neighborsD
  neighborDistances :: [Int]
  neighborDistances = map (\n -> currentDistance + 1) neighbors
  neighborsWithDists = zip3 neighbors neighborDistances (repeat current)
  (newDistances, newQueue) = foldl updateDistance (distances, queue) neighborsWithDists -- if (null neighborsWithDists) then error "neighborsWithDists is empty" else foldl updateDistance (distances, queue) neighborsWithDists
  newVisited = Set.insert current visited
  newCurrent = snd $ Set.findMin newQueue
  finalQueue = Set.deleteMin newQueue
  recurse = dijkstra0 initialMap newDistances newVisited finalQueue newCurrent destination
  in if destination current
        then Just distances
        else if Set.null newQueue
               then Nothing
               else recurse

dijkstra :: BlizzardMap2 -> Node -> TargetFunc -> Maybe Distances
dijkstra initialMap source destination = let
  initialDistances = Map.singleton source (0, undefined)
  initialVisited = Set.empty
  initialQueue = Set.empty
  in dijkstra0 initialMap initialDistances initialVisited initialQueue source destination

traceBack :: Distances -> Node -> Node -> [Node] -> [Node]
traceBack ds source destination accu = let
  (_, next) = lookupOrError destination ds "ds!destination"
  newAccu = next:accu
  in if source == destination then accu else traceBack ds source next newAccu

shortestPath :: BlizzardMap2 -> Node -> TargetFunc -> Maybe ([Node], Distance)
shortestPath initialMap source destination = let
  dijkstraOutput = dijkstra initialMap source destination
  candidates :: Distances -> [Node]
  candidates paths = filter destination $ Map.keys paths
  lookupDist paths n = fst $ paths!n
  destNode paths = head $ sortOn (lookupDist paths) $ candidates paths
  fullLength paths = fst $ lookupOrError (destNode paths) paths "paths!destination"
  path paths = traceBack paths source (destNode paths) []
  in case dijkstraOutput of
    Just paths -> Just (path paths, fullLength paths)
    Nothing -> Nothing

startNode :: BlizzardMap2 -> Node
startNode (BlizzardMap2 (minX, _) (_, maxY) _ _) = ((minX + 1, maxY), 0)

isGoalNode :: BlizzardMap2 -> Node -> Bool
isGoalNode (BlizzardMap2 bl tr _ _) (coords, _) = isGoal bl tr coords

testInput2 = [
    "#.######"
  , "#>>.<^<#"
  , "#.<..<<#"
  , "#>v.><>#"
  , "#<^v^^>#"
  , "######.#"
  ]

solvePart1Pure :: [String] -> Int
solvePart1Pure strs = let
  bMap = parseGrid2 strs
  in snd $ fromJust $ shortestPath bMap (startNode bMap) (isGoalNode bMap)

solvePart1 = do
  text <- readFile "data/input24.txt"
  return $ solvePart1Pure $ lines text

type Blizzard2 = (Int, Direction)

data BlizzardMap2 = BlizzardMap2 {
    getLowerLeft :: Coords
  , getUpperRight :: Coords
  , getHorizontalBlizzards :: Map Int [Blizzard2]
  , getVerticalBlizzards :: Map Int [Blizzard2]
} deriving (Eq, Ord, Show)

parseGrid2 :: [String] -> BlizzardMap2
parseGrid2 strs = let
  (bl, tr, bSet) = parseGrid strs
  isHorizontal (_, d) = d == West || d == East
  (horizontals, verticals) = partition isHorizontal $ Set.toList bSet
  bToB2Horizontal :: (Coords, Direction) -> (Int, (Int, Direction))
  bToB2Horizontal ((x, y), d) = (y, (x, d))
  bToB2Vertical ((x, y), d) = (x, (y, d))
  insertIntoListMap :: Map Int [a] -> (Int, a) -> Map Int [a]
  insertIntoListMap oldMap (key, value) = case Map.lookup key oldMap of
    Nothing -> Map.insert key [value] oldMap
    Just ls -> Map.insert key (value:ls) oldMap
  horizontalM :: Map Int [Blizzard2]
  horizontalM = foldl insertIntoListMap Map.empty $ map bToB2Horizontal horizontals
  verticalM :: Map Int [Blizzard2]
  verticalM = foldl insertIntoListMap Map.empty $ map bToB2Vertical verticals
  in BlizzardMap2 bl tr horizontalM verticalM

hasBlizzard :: BlizzardMap2 -> Coords -> Int -> Bool
hasBlizzard bMap (x, y) turn = let
  BlizzardMap2 (minX, minY) (maxX, maxY) hMap vMap = bMap
  horizontals :: [Blizzard2]
  horizontals = Map.findWithDefault [] y hMap
  verticals = Map.findWithDefault [] x vMap
  hMod = maxX - minX - 1
  vMod = maxY - minY - 1
  occupiedByBlizzard (startCol, East) = x == ((startCol - 1 + turn) `mod` hMod) + 1
  occupiedByBlizzard (startCol, West) = x == ((startCol - 1 - turn) `mod` hMod) + 1
  occupiedByBlizzard (startRow, North) = y == ((startRow -1 + turn) `mod` vMod) + 1
  occupiedByBlizzard (startRow, South) = y == ((startRow -1 - turn) `mod` vMod) + 1
  in any occupiedByBlizzard $ horizontals ++ verticals

solvePart2Pure :: [String] -> (Int, Int, Int, Int)
solvePart2Pure strs = let
  bMap = parseGrid2 strs
  BlizzardMap2 (minX, minY) (maxX, maxY) _ _ = bMap
  goalCoords = (maxX - 1, minY)
  firstDist = snd $ fromJust $ shortestPath bMap (startNode bMap) (isGoalNode bMap)
  startCoords = fst $ startNode bMap
  isStartCoords (coords, turn) = coords == startCoords
  secondDist = snd $ fromJust $ shortestPath bMap (goalCoords, firstDist) isStartCoords
  thirdStartTime = firstDist + secondDist
  thirdDist = snd $ fromJust $ shortestPath bMap (startCoords, thirdStartTime) (isGoalNode bMap)
  in (firstDist, secondDist, thirdDist, firstDist+secondDist+thirdDist)

solvePart2 = do
  text <- readFile "data/input24.txt"
  return $ solvePart2AStarPure $ lines text

-- shortestPathAStar :: (Eq a, Ord a) => a -> NeighborFunc a -> TargetFunc a -> HeuristicFunc a -> ([a], Int)
manhattanDistance :: Coords -> Coords -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

neighborsAStar :: BlizzardMap2 -> Node -> [(Node, Int)]
neighborsAStar bMap node = zip (availableMoves bMap node) (repeat 1)

solvePart2AStarPure :: [String] -> (Int, Int, Int, Int)
solvePart2AStarPure strs = let
  bMap = parseGrid2 strs
  BlizzardMap2 (minX, minY) (maxX, maxY) _ _ = bMap
  goalCoords = (maxX - 1, minY)
  firstDist = snd $ shortestPathAStar (startNode bMap) (neighborsAStar bMap) (isGoalNode bMap) (\n -> manhattanDistance goalCoords (fst n))
  startCoords = fst $ startNode bMap
  isStartCoords (coords, turn) = coords == startCoords
  secondDist = snd $ shortestPathAStar (goalCoords, firstDist) (neighborsAStar bMap) isStartCoords (\n -> manhattanDistance startCoords (fst n))
  thirdStartTime = firstDist + secondDist
  thirdDist = snd $ shortestPathAStar (startCoords, thirdStartTime) (neighborsAStar bMap) (isGoalNode bMap) (\n -> manhattanDistance goalCoords (fst n))
  in (firstDist, secondDist, thirdDist, firstDist+secondDist+thirdDist)

solvePart2BFSPure :: [String] -> (Int, Int, Int, Int)
solvePart2BFSPure strs = let
  bMap = parseGrid2 strs
  BlizzardMap2 (minX, minY) (maxX, maxY) _ _ = bMap
  goalCoords = (maxX - 1, minY)
  firstDist = snd $ shortestPathBFS (startNode bMap) (availableMoves bMap) (isGoalNode bMap)
  startCoords = fst $ startNode bMap
  isStartCoords (coords, turn) = coords == startCoords
  secondDist = snd $ shortestPathBFS (goalCoords, firstDist) (availableMoves bMap) isStartCoords
  thirdStartTime = firstDist + secondDist
  thirdDist = snd $ shortestPathBFS (startCoords, thirdStartTime) (availableMoves bMap) (isGoalNode bMap)
  in (firstDist, secondDist, thirdDist, firstDist+secondDist+thirdDist)

