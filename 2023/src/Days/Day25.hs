module Days.Day25 (runDay) where

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
lineParser :: Parser (String, [String])
lineParser = do
  source <- many1 letter
  string ": "
  dests <- many1 letter `sepBy` char ' '
  return (source, dests)

inputParser :: Parser Input
inputParser = lineParser `sepBy` endOfLine

------------ TYPES ------------

type Weight = Int
type NeighborFunc a = a -> [(a, Weight)]
type Graph a = Map a [(a, Weight)]
type Input = [(String, [String])]

type OutputA = Void

type OutputB = Void

------------ PART A ------------

-- this could be O(n) if I were sure they'd be sorted but I'm not so O(n log n)
mergeEdgeWeights :: Ord a => [(a, Weight)] -> [(a, Weight)] -> [(a, Weight)]
mergeEdgeWeights xs ys = let
  xMap = Map.fromListWith (+) xs
  yMap = Map.fromListWith (+) ys
  in Map.toList $ Map.unionWith (+) xMap yMap

graphToEdges :: Graph String -> [(String, String, Weight)]
graphToEdges graph = let
  pairs = Map.toList graph
  pairToEdges (source, dests) = map (\(dest, weight) -> (source, dest, weight)) dests
  in concat $ map pairToEdges pairs

relabelEdge :: String -> String -> String -> (String, String, Weight) -> Maybe (String, String, Weight)
relabelEdge old1 old2 new (from, to, weight)
  | fromOld && toOld = Nothing
  | fromOld = Just (new, to, weight)
  | toOld = Just (from, new, weight)
  | otherwise = Just (from, to, weight)
  where
    fromOld = from == old1 || from == old2
    toOld = to == old1 || to == old2

mergeTwoVertices :: Map String [(String, Weight)] -> (String, String) -> Map String [(String, Weight)]
mergeTwoVertices oldMap (v1, v2) = let
  newLabel = v1 ++ v2
  oldEdges = graphToEdges oldMap
  newEdges = catMaybes $ map (relabelEdge v1 v2 newLabel) oldEdges
  insertEdgeTemp :: Map (String, String) Weight -> (String, String, Weight) -> Map (String, String) Weight
  insertEdgeTemp m (from, to, weight) = Map.insertWith (+) (from, to) weight m
  tempEdges :: Map (String, String) Weight
  tempEdges = foldl insertEdgeTemp Map.empty newEdges
  betterEdges = map (\((a, b), c) -> (a, b, c)) $ Map.toList tempEdges
  in graphFromEdges betterEdges

-- visited 2 queue is 2/1 2/5 2/6 3/3
-- visited 2,3 queue 2/1 2/5 2/6 4/4 2/7
-- visited 234 queue 2/1 2/5 2/6 2+2/7 2/8
-- visited 2347 queue 2/1 2/5 2+1/6 2+3/8
-- visited 23478 queue 2/1 2/5 3/6
-- visited 234786 queue 2/1 2+3/5
minimumCutPhase0 :: (Ord a, Show a) => NeighborFunc a -> Set a -> Set (Weight, a) -> (a, a)
minimumCutPhase0 neighbors unqueued queue0
  | unqueuedSize == 0 && queueSize < 2 = error "how did the queue get empty?"
  | unqueuedSize == 0 && queueSize == 2 = endPhase
  | otherwise = processNext
  where
    queue = traceShow ("mcp0 unqueued=" ++ show unqueued ++ " queue=" ++ show queue0) queue0
    queueSize = Set.size queue
    unqueuedSize = Set.size unqueued
    ((_, v), queue2) = Set.deleteFindMax queue
    unqueued2 = unqueued -- sorry
    -- for each node in the queue, if it has v as a neighbor, we need to increase its
    -- key by the weight to v
    edgesToV w = filter (\p -> fst p == v) $ neighbors w
    weightToAdd w = sum $ map snd $ edgesToV w
    updateKey (k, w) = (k + weightToAdd w, w)
    queue3 = Set.map updateKey queue2
    fromV = neighbors v
    newNodes = filter (\p -> Set.member (fst p) unqueued2) fromV
    unqueued3 = foldl (flip Set.delete) unqueued2 (map fst newNodes)
    swap (x, y) = (y, x)
    queue4 = Set.union queue3 $ Set.fromList $ map swap newNodes
    processNext = minimumCutPhase0 neighbors unqueued3 queue4
    -- the remaining statements are evaluated when queueSize == 2
    [t, s] = Set.toList queue
    endPhase = (snd s, snd t)

minimumCutPhase :: (Ord a, Show a) => NeighborFunc a -> Set a -> a -> (a, a)
-- come back to this: we can probably just pass in a graph but we haven't
-- thought out the graph types yet
minimumCutPhase neighbors vertices start = let
  initialQueue = Set.singleton (0, start)
  in minimumCutPhase0 neighbors (Set.delete start vertices) initialQueue

minimumCut0 :: Map String [(String, Weight)] -> String ->  (String, String, Int) -> (String, String, Int)
minimumCut0 graph start (bestS, bestT, bestWeight) = let
  dumbFunc :: String -> [(String, Weight)]
  dumbFunc v = Map.findWithDefault (error ("you fail as usual but in minimumCut0 this time: " ++ v)) v graph
  (s, t) = minimumCutPhase dumbFunc (Map.keysSet graph) start
  weightsFromT :: Int
  weightsFromT = sum $ map snd $ dumbFunc t
  newBest0 = if weightsFromT < bestWeight then (s, t, weightsFromT) else (bestS, bestT, bestWeight)
  newBest = traceShow ("last s&t are " ++ show (s, t, weightsFromT) ++ " so new best is " ++ show newBest0) newBest0
  newGraph = mergeTwoVertices graph (s, t)
  recurse = minimumCut0 newGraph start newBest
  in if Map.size graph < 4 then newBest else recurse

minimumCut :: Map String [(String, Weight)] -> String -> (String, String, Int)
minimumCut graph start = minimumCut0 graph start (undefined, undefined, maxBound)

-- https://dl.acm.org/doi/pdf/10.1145/263867.263872
testEdges = [
    ("1", [("2", 2), ("5", 3)])
  , ("2", [("3", 3), ("5", 2), ("6", 2)])
  , ("3", [("4", 4), ("7", 2)])
  , ("4", [("7", 2), ("8", 2)])
  , ("5", [("6", 3)])
  , ("6", [("7", 1)])
  , ("7", [("8", 3)])
  , ("8", [])
  ]
addReverseEdges :: [(a, [(a, Weight)])] -> [(a, a, Weight)]
addReverseEdges ls = let
  flipEdge (source, dest, weight) = (dest, source, weight)
  expandEdges (source, dests) = map (\(dest, weight) -> (source, dest, weight)) dests
  expanded = concat $ map expandEdges ls
  in expanded ++ (map flipEdge expanded)

graphFromEdges :: Ord a => [(a, a, Weight)] -> Map a [(a, Weight)]
graphFromEdges edges = let
  insertEdge g (source, dest, weight) = case Map.lookup source g of
    Just result -> Map.insert source ((dest, weight):result) g
    Nothing -> Map.insert source [(dest, weight)] g
  in foldl insertEdge Map.empty edges

testGraph = graphFromEdges $ addReverseEdges testEdges
testNeighbors v = Map.findWithDefault (error "you fail as usual") v testGraph
testResult = minimumCut testGraph "2"


-- partA :: Input -> OutputA
partA input = testResult

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
