module BreadthFirstSearch (shortestPathBFS) where

import Common
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type TargetFunc a = a -> Bool
type NeighborFunc a = a -> [a]

data BFSState a = BFSState {
    getQueue :: Queue a
  , getParents :: Map a a
  , getExplored :: Set a
}

bfs0 :: (Ord a) => BFSState a -> NeighborFunc a -> TargetFunc a -> (a, Map a a)
bfs0 (BFSState queue parents explored) findNeighbors isGoal = let
  (current, queue2) = popQ queue
  neighbors = findNeighbors current
  unexploredNeighbors = filter (\n -> Set.notMember n explored) neighbors
  finalParents = Map.union (Map.fromList $ zip unexploredNeighbors (repeat current)) parents
  finalExplored = Set.union explored (Set.fromList unexploredNeighbors)
  finalQueue = foldl pushQ queue2 unexploredNeighbors
  recurse = bfs0 (BFSState finalQueue finalParents finalExplored) findNeighbors isGoal
  in if (isGoal current || emptyQ finalQueue)
    then (current, parents)
    else recurse

bfs :: (Ord a) => a -> NeighborFunc a -> TargetFunc a -> (a, Map a a)
bfs startNode findNeighbors isGoal = let
  initialQueue = fromListQ [startNode]
  initialExplored = Set.singleton startNode
  initialParents = Map.empty
  initialState = BFSState initialQueue initialParents initialExplored
  in bfs0 initialState findNeighbors isGoal

shortestPathBFS :: (Eq a, Ord a, Show a) => a -> NeighborFunc a -> TargetFunc a -> ([a], Int)
shortestPathBFS startNode findNeighbors isGoal = let
  (goalNode, parents) = bfs startNode findNeighbors isGoal
  path = reverse (traceBack parents startNode goalNode)
  in (path, length path - 1)

traceBack :: (Eq a, Ord a) => Map a a -> a -> a -> [a]
traceBack parents startNode destNode =
  if startNode == destNode
    then [startNode]
    else destNode:(traceBack parents startNode (parents!destNode))

exampleMap :: Map Char [Char]
exampleMap = Map.fromList [
    ('a', ['b', 'c', 'e'])
  , ('b', ['c'])
  , ('c', ['e', 'd'])
  , ('d', ['e', 'f'])
  , ('e', ['f'])
  ]

exampleFind :: Char -> [Char]
exampleFind c = exampleMap!c

testBFS = shortestPathBFS 'a' exampleFind (== 'f')