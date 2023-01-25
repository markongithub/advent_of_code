module AStar (shortestPathAStar) where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type FScore = Int
type GScore = Int
type GScores a = Map a (GScore, a)
type OpenSet a = Set (FScore, a)
type TargetFunc a = a -> Bool
type NeighborFunc a = a -> [(a, Int)]
type HeuristicFunc a = a -> FScore

data AStarState a = AStarState {
    getOpenSet :: OpenSet a
  , getGScores :: GScores a
  , getCurrent :: a
}

aStar0 :: (Eq a, Ord a) => AStarState a -> NeighborFunc a -> TargetFunc a -> HeuristicFunc a -> (a, GScores a)
aStar0 (AStarState openSet gScores current) findNeighbors isGoal heuristic = let
  -- if newOpenSet is empty then return gScores
  -- if isGoal current then return gScores
  -- neighborsWithDists :: (Eq a, Ord a) => [(a, Int)]
  neighborsWithDists = findNeighbors current
  currentG = gScores!current
  -- neighborsWithTentativeGs :: [(a, GScore)]
  neighborsWithTentativeGs = map (\(node, dist) -> (node, fst currentG + dist)) neighborsWithDists
  -- updateFunc :: (OpenSet a, GScores a) -> (a, GScore) -> (OpenSet a, GScores a)
  updateFunc (osInner, gsInner) (node, tentativeGScore) = let
    better = Map.notMember node gsInner || tentativeGScore < (fst $ (gsInner!node))
    in if better
      then (Set.insert (tentativeGScore + (heuristic node), node) osInner, Map.insert node (tentativeGScore, current) gsInner)
      else (osInner, gsInner)
  (openSet2, gScoresFinal) = foldl updateFunc (openSet, gScores) neighborsWithTentativeGs
  -- nextCurrent :: (FScore, a)
  (nextCurrent,  openSet3) = Set.deleteFindMin openSet2
  -- recurse :: GScores a
  recurse = aStar0 (AStarState openSet3 gScoresFinal (snd nextCurrent)) findNeighbors isGoal heuristic
  in if (isGoal current || Set.null openSet2)
    then (current, gScores)
    else recurse

aStar :: (Eq a, Ord a) => a -> NeighborFunc a -> TargetFunc a -> HeuristicFunc a -> (a, GScores a)
aStar startNode findNeighbors isGoal heuristic = let
  gScores = Map.fromList [(startNode,(0, undefined))]
  state = AStarState Set.empty gScores startNode
  in aStar0 state findNeighbors isGoal heuristic

shortestPathAStar :: (Eq a, Ord a) => a -> NeighborFunc a -> TargetFunc a -> HeuristicFunc a -> ([a], Int)
shortestPathAStar startNode findNeighbors isGoal heuristic = let
  (goalNode, gScores) = aStar startNode findNeighbors isGoal heuristic
  in (reverse (traceBack gScores startNode goalNode), fst (gScores!goalNode))

traceBack :: (Eq a, Ord a) => GScores a -> a -> a -> [a]
traceBack gScores startNode destNode =
  if startNode == destNode
    then [startNode]
    else destNode:(traceBack gScores startNode (snd (gScores!destNode)))

exampleMap :: Map Char [(Char, Int)]
exampleMap = Map.fromList [
    ('a', [('b', 1), ('c', 1), ('e', 1)])
  , ('b', [('c', 1)])
  , ('c', [('e', 1), ('d', 1)])
  , ('d', [('e', 1), ('f', 1)])
  , ('e', [('f', 1)])
  ]

exampleFind :: Char -> [(Char, Int)]
exampleFind c = exampleMap!c

exampleHeuristic c = 1

testAStar = aStar 'a' exampleFind (== 'f') exampleHeuristic