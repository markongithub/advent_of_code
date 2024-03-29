module Day12 where

import Data.Char (isUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Node = Start | BigCave String | SmallCave String | End
  deriving (Eq, Ord, Show)

type CaveGraph = Map Node (Set Node)

data SearchState = SearchState {
    graphF :: CaveGraph
  , currentPathF :: [Node]
  , smallCavesSeenF :: Maybe (Set Node)
  }

isSmallCave n = case n of
  SmallCave _ -> True
  _           -> False

listPaths0 :: SearchState -> [[Node]]
listPaths0 (SearchState graph currentPath cavesSeen)
  | currentNode == End = [currentPath]
  | otherwise = concat $ map listPaths0 childStates
  where
    currentNode = head currentPath
    newGraph = if (isSmallCave currentNode) then updateGraph else graph
    updateGraph = case cavesSeen of
      Nothing -> Map.delete currentNode graph
      Just s -> if (Set.member currentNode s) then blockAllVisitedCaves s else graph
    newCavesSeen = case cavesSeen of
      Nothing -> Nothing
      Just s -> case Set.member currentNode s of
        True -> Nothing
        False -> if (isSmallCave currentNode) then Just $ Set.insert currentNode s else cavesSeen
    nextNodes = filter (/= Start) $ Set.toList $ Map.findWithDefault Set.empty currentNode graph
    makeNewState newNode = SearchState newGraph (newNode:currentPath) newCavesSeen
    childStates :: [SearchState]
    childStates = map makeNewState nextNodes
    blockAllVisitedCaves s = foldl (flip Map.delete) graph (Set.toList s)

listPaths :: CaveGraph -> Node -> Bool -> [[Node]]
listPaths g n isPart2 = let
  initialSet = if isPart2 then Just Set.empty else Nothing
  in listPaths0 $ SearchState g [n] initialSet

countPaths :: CaveGraph -> Node -> Bool -> Int
countPaths g n b = length $ listPaths g n b

parseNode :: String -> Node
parseNode s
  | isUpper (head s) = BigCave s
  | otherwise = case s of
    "start" -> Start
    "end" -> End
    _-> SmallCave s

type Edge = (Node, Node)

parseEdge :: String -> Edge
parseEdge s = let
  firstNodeStr = takeWhile (/= '-') s
  secondNodeStr = drop (length firstNodeStr + 1) s
  in (parseNode firstNodeStr, parseNode secondNodeStr)

insertEdge :: CaveGraph -> Edge -> CaveGraph
insertEdge g (from, to) = Map.insertWith Set.union from (Set.singleton to) g

insertEdgeBidir :: CaveGraph -> Edge -> CaveGraph
insertEdgeBidir g (from, to) = let
  g2 = insertEdge g (from, to)
  in insertEdge g2 (to, from) 

graphFromEdges :: [Edge] -> CaveGraph
graphFromEdges edges = foldl insertEdgeBidir Map.empty edges

testData1 = [
    "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]

graph1 = graphFromEdges $ map parseEdge testData1

testData3 = [
    "fs-end"
  , "he-DX"
  , "fs-he"
  , "start-DX"
  , "pj-DX"
  , "end-zg"
  , "zg-sl"
  , "zg-pj"
  , "pj-he"
  , "RW-he"
  , "fs-DX"
  , "pj-RW"
  , "zg-RW"
  , "start-pj"
  , "he-WI"
  , "zg-he"
  , "pj-fs"
  , "start-RW"
  ]

graph3 = graphFromEdges $ map parseEdge testData3

puzzleInput1 = [
    "ey-dv"
  , "AL-ms"
  , "ey-lx"
  , "zw-YT"
  , "hm-zw"
  , "start-YT"
  , "start-ms"
  , "dv-YT"
  , "hm-ms"
  , "end-ey"
  , "AL-ey"
  , "end-hm"
  , "rh-hm"
  , "dv-ms"
  , "AL-dv"
  , "ey-SP"
  , "hm-lx"
  , "dv-start"
  , "end-lx"
  , "zw-AL"
  , "hm-AL"
  , "lx-zw"
  , "ey-zw"
  , "zw-dv"
  , "YT-ms"
  ]
puzzle1 = graphFromEdges $ map parseEdge puzzleInput1

solvePart1 = countPaths puzzle1 Start False
solvePart2 = countPaths puzzle1 Start True
