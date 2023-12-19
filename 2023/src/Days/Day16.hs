module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace (traceShow)
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
data Direction = North | South | West | East deriving (Eq, Ord, Show)


type OutputA = Int

type OutputB = Int

------------ PART A ------------

move :: Coords -> Direction -> Coords
move (x,y) dir = case dir of
  North -> (x, y + 1)
  South -> (x, y - 1)
  West -> (x - 1, y)
  East -> (x + 1, y)

inBounds :: Coords -> Coords -> Bool
inBounds (maxX, maxY) (x, y) = x >=0 && y >=0 && x <= maxX && y <= maxY

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West

hitMirror :: Direction -> Char -> Direction
hitMirror North '/' = East
hitMirror East '/' = North
hitMirror South '/' = West
hitMirror West '/' = South
hitMirror dir '\\' = opposite $ hitMirror dir '/'
hitMirror dir c = error ("bad mirror: " ++ [c])

hitSplitter :: Direction -> Char -> [Direction]
hitSplitter d c = let
  firstDir North '|' = Nothing
  firstDir North '-' = Just West
  firstDir South c2 = firstDir North c2
  firstDir West '|' = Just North
  firstDir West '-' = Nothing
  firstDir East c2 = firstDir West c2
  firstDir d2 c2 = error ("unexpected splitter: " ++ show (d2, c2))
  in case firstDir d c of
    Nothing -> [d]
    Just dOut -> [dOut, opposite dOut]

isMirror :: Char -> Bool
isMirror '/' = True
isMirror '\\' = True
isMirror _ = False

isSplitter :: Char -> Bool
isSplitter '-' = True
isSplitter '|' = True
isSplitter _ = False

trackBeam :: Input -> Coords -> Set (Coords, Direction) -> (Coords, Direction) -> Set (Coords, Direction)
trackBeam graph bounds explored (coords, dir) = let
  thisCell = Map.findWithDefault '.' coords graph
  nextDirections = case thisCell of
    '.' -> [dir]
    '/' -> [hitMirror dir thisCell]
    '\\' -> [hitMirror dir thisCell]
    '-' -> hitSplitter dir thisCell
    '|' -> hitSplitter dir thisCell
    x -> error ("bad cell content: " ++ [x])
  candidates = map (move coords) nextDirections
  candidatePairs :: [(Coords, Direction)]
  candidatePairs = zip candidates nextDirections
  destinations = filter (\(c1, d1) -> inBounds bounds c1) candidatePairs
  goDest e1 (c1, d1) = trackBeam graph bounds e1 (c1,d1)
  newExplored = Set.insert (coords, dir) explored
  goAllDests = foldl goDest newExplored destinations
  in case Set.member (coords, dir) explored of
    True -> explored
    False -> goAllDests

findBounds :: Input -> Coords
findBounds input = let
  allCoords = Map.keys input
  (allX, allY) = unzip allCoords
  in (maximum allX, maximum allY)


partA :: Input -> OutputA
partA input = let
  bounds = findBounds input
  firstCoords = (0, snd bounds)
  reachableCoords = Set.toList $ Set.fromList $ map fst $ Set.toList $ trackBeam input bounds Set.empty (firstCoords, East)
  in length reachableCoords

------------ PART B ------------
type Vertex = (Coords, Direction)

getNeighbors :: Input -> Coords -> Vertex -> [Vertex]
getNeighbors graph bounds (coords, dir) = let
  thisCell = Map.findWithDefault '.' coords graph
  nextDirections = case thisCell of
    '.' -> [dir]
    '/' -> [hitMirror dir thisCell]
    '\\' -> [hitMirror dir thisCell]
    '-' -> hitSplitter dir thisCell
    '|' -> hitSplitter dir thisCell
    x -> error ("bad cell content: " ++ [x])
  candidates = map (move coords) nextDirections
  candidatePairs :: [(Coords, Direction)]
  candidatePairs = zip candidates nextDirections
  in filter (\(c1, d1) -> inBounds bounds c1) candidatePairs


data TarjanState v = TarjanState {
    getLow :: Map v Int
  , getDisc :: Map v Int
  , getVisited :: Set v
  , getNextIndex :: Int
  , getStack :: [v]
  , getStackSet :: Set v
  , getSCCs :: [[v]]
  } deriving (Eq, Show)

initialState = TarjanState Map.empty Map.empty Set.empty 0 [] Set.empty []

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore _ [] = []
takeWhileOneMore f (x:xs) = case f x of
  True -> x:(takeWhileOneMore f xs)
  False -> [x]

tarjan0 :: (Eq v, Ord v, Show v) => (v -> [v]) -> TarjanState v -> v -> TarjanState v
tarjan0 neighbors state0 u = let
  state = state0 -- traceShow ("tarjan0 " ++ show u) state0
  newDisc = Map.insert u (getNextIndex state) $ getDisc state
  newLow = Map.insert u (getNextIndex state) $ getLow state
  newIndex = 1 + getNextIndex state
  newStack = u:(getStack state)
  newStackSet = Set.insert u (getStackSet state)
  vs = neighbors u
  stateBeforeRecursions = state { getDisc = newDisc, getLow = newLow, getNextIndex = newIndex, getStack = newStack, getStackSet = newStackSet}
  maybeRecurseNeighbor s0 v = if Map.member v (getDisc s0) then s0 else tarjan0 neighbors s0 v
  stateAfterRecursions = foldl maybeRecurseNeighbor stateBeforeRecursions vs
  unvisited = filter (\v -> Map.notMember v (getDisc state)) vs
  visitedAndOnStack = filter (\v -> Map.member v (getDisc state) && Set.member v (getStackSet state)) vs
  findLowOrError v = Map.findWithDefault (error ("no low for " ++ show v)) v $ getLow stateAfterRecursions
  lowsFromStack :: [Int]
  lowsFromStack = map (\v -> ((getDisc stateAfterRecursions)!v)) visitedAndOnStack
  candidateLows :: [Int]
  candidateLows = [findLowOrError u] ++ (map findLowOrError unvisited) ++ lowsFromStack
  newLowU :: Int
  newLowU = minimum candidateLows
  newDiscU = (getDisc stateAfterRecursions)!u
  thisSCC = if newLowU == newDiscU then takeWhileOneMore (/= u) (getStack stateAfterRecursions) else []
  finalStack = drop (length thisSCC) (getStack stateAfterRecursions)
  finalStackSet = foldl (flip Set.delete) (getStackSet stateAfterRecursions) thisSCC
  finalSCCs = if null thisSCC then (getSCCs stateAfterRecursions) else thisSCC:(getSCCs stateAfterRecursions)
  finalLow = Map.insert u newLowU (getLow stateAfterRecursions)
  in stateAfterRecursions {getLow = finalLow, getStack = finalStack, getStackSet = finalStackSet, getSCCs = finalSCCs}

tarjan :: (Eq v, Ord v, Show v) => (v -> [v]) -> [v] -> [[v]]
tarjan neighbors us = let
  processOne s0 u = tarjan0 neighbors s0 u
  in getSCCs $ foldl processOne initialState us

testGraph = Map.fromList [
    ('A', ['B'])
  , ('B', ['C', 'D'])
  , ('C', ['A'])
  , ('D', ['E'])
  , ('E', [])
  ]

testFunc v = testGraph!v
testOutput = tarjan0 testFunc initialState 'A'

valueCounts :: (Ord k, Show k) => [k] -> Map k Int
valueCounts ls = let
  addToMap m v = Map.insertWith (+) v 1 m
  in foldl addToMap Map.empty ls

type SCCGraph = Map Int (Set Int)
addEdge :: SCCGraph -> (Int, Int) -> SCCGraph
addEdge graph (from, to) = Map.insertWith Set.union from (Set.singleton to) graph

nodeToSCC :: Ord v =>  [[v]] -> (Map Int [v], Map v Int)
nodeToSCC sccls = let
  nodeListsWithSCCIndices = zip [0..] sccls
  nodesWithSCCIndices = concat $ map (\(scc, ls) -> zip ls (repeat scc)) nodeListsWithSCCIndices
--  nodeToSCCOutput :: Map v Int
  nodeToSCCOutput = Map.fromList nodesWithSCCIndices
  in (Map.fromList nodeListsWithSCCIndices, nodeToSCCOutput)

makeSCCGraph :: Ord v => (v -> [v]) -> (Map Int [v], Map v Int) -> SCCGraph
makeSCCGraph oldNeighbors (sccsByIndex, sccsByNode) = let
  addOneSCC graph i = let
    allDestNodes = concat $ map oldNeighbors (sccsByIndex!i)
    allDestSCCs = Set.fromList $ map (\n -> sccsByNode!n) allDestNodes
    in Map.insert i allDestSCCs graph
  in foldl addOneSCC Map.empty (Map.keys sccsByIndex)

-- this isn't tail-recursive and it doesn't do an ordering but let's try it for now
depthFirstSearch :: Ord v => (v -> [v]) -> Set v -> v -> Set v
depthFirstSearch neighbors visited v = let
  candidates = neighbors v
  nextVisited = Set.insert v visited
  maybeRecurse s0 w = if Set.member w s0 then s0 else depthFirstSearch neighbors s0 w
  in foldl maybeRecurse nextVisited candidates

reachable :: Ord v => (Map Int [v], Map v Int) -> (Int -> [Int]) -> v -> [v]
reachable (sccsByIndex, sccsByNode) sccNeighbors v = let
  mySCC = sccsByNode!v
  reachableSCCs = depthFirstSearch sccNeighbors Set.empty mySCC
  in concat $ map (\scc -> sccsByIndex!scc) $ Set.toList reachableSCCs

partB :: Input -> OutputB
partB input = let
  bounds = findBounds input
  leftCoords = zip (repeat 0) [0..(snd bounds)]
  leftVertices = zip leftCoords (repeat East)
  topCoords = zip [0..(fst bounds)] (repeat $ snd bounds)
  topVertices = zip topCoords (repeat South)
  rightVertices = zip (zip (repeat $ fst bounds) [0..(snd bounds)]) (repeat West)
  bottomVertices = zip (zip [0..(fst bounds)] (repeat 0)) (repeat North)
  startVertices = leftVertices ++ topVertices ++ bottomVertices ++ rightVertices
  neighborFunc = getNeighbors input bounds
  sccs = tarjan neighborFunc startVertices
  (sccsByIndex, sccsByNode) = nodeToSCC sccs
  sccGraph = makeSCCGraph neighborFunc (sccsByIndex, sccsByNode)
  sccNeighborFunc = (\v -> Set.toList $ sccGraph!v)
  reachableNodes v = reachable (sccsByIndex, sccsByNode) sccNeighborFunc v
  dedupCoords :: [Vertex] -> [Coords]
  dedupCoords vertices = Set.toList $ Set.fromList $ map fst vertices
  numReachableCoords v = length $ dedupCoords $ reachableNodes v
  in maximum $ map numReachableCoords startVertices
  