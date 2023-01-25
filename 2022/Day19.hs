module Day19 where

import Data.List (maximumBy, sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)
type Inventory = Map Resource Int
data State = State {
    getRobots :: Inventory
  , getInventory :: Inventory
  , getCurMinute :: Int
  } deriving (Eq, Ord, Show)

type Price = Map Resource Int

type Blueprint = Map Resource Price

doMining :: State -> State
doMining (State robots inventory time) = let
  updateInventory :: Inventory -> (Resource, Int) -> Inventory
  updateInventory imap (res, q) = Map.insertWith (+) res q imap
  finalInventory = foldl updateInventory inventory (Map.toList robots)
  in State robots finalInventory (time + 1)

canBuyRobot :: Blueprint -> State -> Resource -> Bool
canBuyRobot blueprint (State _ inventory _) robotType = let
  price = blueprint!robotType
  canAfford (res, q) = (Map.findWithDefault 0 res inventory) >= q
  in all canAfford $ Map.toList price

buyRobot :: Blueprint -> State -> Resource -> State
buyRobot blueprint state robotType = let
  (State robots inventory t) = state
  price = blueprint!robotType
  subtractRes :: Inventory -> (Resource, Int) -> Inventory
  subtractRes someInv (res, q) = Map.adjust (\stupid -> stupid - q) res someInv
  newInventory = foldl subtractRes inventory $ Map.toList price
  newRobots :: Inventory
  newRobots = Map.insertWith (+) robotType 1 robots
  in State newRobots newInventory t
 
initialState = State (Map.singleton Ore 1) Map.empty 1

testBlueprint :: Blueprint
testBlueprint = Map.fromList [
    (Ore, Map.fromList [(Ore, 4)])
  , (Clay, Map.fromList [(Ore, 2)])
  , (Obsidian, Map.fromList [(Ore, 3), (Clay, 14)])
  , (Geode, Map.fromList [(Ore, 2), (Obsidian, 7)])
  ]

evaluate :: Blueprint -> Int -> State -> Int
evaluate bp depth state = let
  State _ inventory time = state
  options :: [State]
  options = availableOptions bp state
  options2 = if (null options) then error "null options" else options
  recurse = maximum $ map (evaluate bp (depth - 1)) options2
  in if (depth <= 0 || time >= 25)
    then Map.findWithDefault 0 Geode inventory
    else recurse

testBuy :: Resource -> State -> State
testBuy res s = waitAndBuyRobot testBlueprint s res

applyPath :: State -> [(State -> State)] -> State
applyPath s [] = s
applyPath s (f:fs) = applyPath (f s) fs

type Node = State
type Distance = Int
type NodeQueue = Set (Distance, Node)
type Distances = Map Node (Distance, Node)
type TargetFunc = Node -> Bool

findNeighbors :: Blueprint -> Node -> [Node]
findNeighbors bp = availableOptions bp

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

dijkstra0 :: Blueprint -> Distances -> Set Node -> Set (Int, Node) -> Node -> TargetFunc -> Maybe Distances
dijkstra0 blueprint distances visited queue current destination = let
  currentDistance = fst $ lookupOrError current distances "distances!current"
  neighborsD = findNeighbors blueprint current
  neighbors = neighborsD -- if null neighborsD then error ("no neighbors from " ++ show current) else neighborsD
  neighborDistances :: [Int]
  neighborDistances = map (\n -> currentDistance + getCurMinute n - getCurMinute current) neighbors
  neighborsWithDists = zip3 neighbors neighborDistances (repeat current)
  (newDistances, newQueue) = foldl updateDistance (distances, queue) neighborsWithDists -- if (null neighborsWithDists) then error "neighborsWithDists is empty" else foldl updateDistance (distances, queue) neighborsWithDists
  newVisited = Set.insert current visited
  newCurrent = snd $ Set.findMin newQueue
  finalQueue = Set.deleteMin newQueue
  recurse = dijkstra0 blueprint newDistances newVisited finalQueue newCurrent destination
  in if destination current
        then Just distances
        else if Set.null newQueue
               then Nothing
               else recurse

dijkstra :: Blueprint -> Node -> TargetFunc -> Maybe Distances
dijkstra blueprint source destination = let
  initialDistances = Map.singleton source (0, undefined)
  initialVisited = Set.empty
  initialQueue = Set.empty
  in dijkstra0 blueprint initialDistances initialVisited initialQueue source destination

traceBack :: Distances -> Node -> Node -> [Node] -> [Node]
traceBack ds source destination accu = let
  (_, next) = lookupOrError destination ds "ds!destination"
  newAccu = next:accu
  in if source == destination then accu else traceBack ds source next newAccu

shortestPath :: Blueprint -> Node -> TargetFunc -> Maybe ([Node], Distance)
shortestPath blueprint source destination = let
  dijkstraOutput = dijkstra blueprint source destination
  candidates :: Distances -> [Node]
  candidates paths = filter destination $ Map.keys paths
  lookupDist paths n = fst $ paths!n
  destNode paths = head $ sortOn (lookupDist paths) $ candidates paths
  fullLength paths = fst $ lookupOrError (destNode paths) paths "paths!destination"
  path paths = traceBack paths source (destNode paths) []
  in case dijkstraOutput of
    Just paths -> Just (path paths, fullLength paths)
    Nothing -> Nothing

findNode :: Blueprint -> Node -> TargetFunc -> Node
findNode blueprint source destination = let
  dijkstraOutput = dijkstra blueprint source destination
  candidates :: Distances -> [Node]
  candidates paths = filter destination $ Map.keys paths
  lookupDist paths n = fst $ paths!n
  destNode paths = head $ sortOn (lookupDist paths) $ candidates paths
  finalPaths = fromJust dijkstraOutput
  in destNode finalPaths

hasGeodeBot :: TargetFunc
hasGeodeBot (State robots _ _) = Map.findWithDefault 0 Geode robots >= 1

shortestPathToGeode :: Blueprint -> Node -> ([Node], Distance)
shortestPathToGeode blueprint state = fromJust $ shortestPath blueprint state hasGeodeBot

distanceToPrice :: Blueprint -> State -> Resource -> Maybe Int
distanceToPrice blueprint (State robots inventory _) res = let
  price = blueprint!res
  necessaryResources = if (null $ Map.keys price) then error (show price) else Map.keys price
  deficit res = price!res - Map.findWithDefault 0 res inventory
  haveRobot res = Map.findWithDefault 0 res robots >= 1
  haveAllRobots = all haveRobot necessaryResources
  divideAndRoundUp x y = (x + y - 1) `div` y
  turnsToRes res = divideAndRoundUp (deficit res) (robots!res)
  maximumTurns = maximum $ map turnsToRes necessaryResources
  in if haveAllRobots then Just maximumTurns else Nothing

canBuildPrice :: Blueprint -> State -> Resource -> Bool
canBuildPrice blueprint state res = let
  (State _ _ time) = state
  in case distanceToPrice blueprint state res of
    Nothing -> False
    Just t -> time + t < 24

waitAndBuyRobot :: Blueprint -> State -> Resource -> State
waitAndBuyRobot blueprint state res = let
  turnsToWait = fromJust $ distanceToPrice blueprint state res
  afterWaiting = head $ drop (turnsToWait + 1) $ iterate doMining state
  in buyRobot blueprint afterWaiting res

waitForEnd :: State -> State
waitForEnd state = let
  (State _ _ time) = state
  timeLeft = 25 - time
  in head $ drop timeLeft $ iterate doMining state

availableOptions :: Blueprint -> State -> [State]
availableOptions blueprint oldState = let
  thingsToBuy = Set.fromList $ filter (canBuildPrice blueprint oldState) $ Map.keys blueprint
  giveUp = waitForEnd oldState
  afterBuying = map (waitAndBuyRobot blueprint oldState) $ Set.toList thingsToBuy
  -- in if getCurMinute oldState >= 25 then [] else giveUp:afterBuying
  in if getCurMinute oldState >= 25
    then []
    else if null afterBuying
      then [giveUp]
      else afterBuying
--      else if Set.member Geode thingsToBuy
--        then [waitAndBuyRobot blueprint oldState Geode]
--        else afterBuying

obsidianThenGeo :: Blueprint -> State -> (State, State)
obsidianThenGeo blueprint state1 = let
  state2 = findNode blueprint state1 (\s -> Map.findWithDefault 0 Obsidian (getRobots s) >0)
  state3 = findNode blueprint state2 (\s -> Map.findWithDefault 0 Geode (getRobots s) >0)
  in (state2, state3)

testPath2 :: [(State -> State)]
testPath2 = [
    testBuy Clay --3 
  , testBuy Clay --5
  , testBuy Clay -- 7
  , testBuy Obsidian --11
  , testBuy Clay
  , testBuy Obsidian --15
  , testBuy Geode --18
  , testBuy Geode --21
  ]

tiebreakers :: Ord b => [(a -> b)] -> a -> a -> Ordering
tiebreakers [] s1 s2 = GT -- this is a cop-out
tiebreakers (f:fs) s1 s2 = if (f s1) /= (f s2)
                       then compare (f s1) (f s2)
                       else tiebreakers fs s1 s2

rankStates :: State -> State -> Ordering
rankStates s1 s2 = let
  countResource r s = Map.findWithDefault 0 r (getInventory s)
  countBots :: Resource -> State -> Int
  countBots r s = Map.findWithDefault 0 r (getRobots s)
  funcs :: [(State -> Int)]
  funcs = [
      (\s -> -1 * getCurMinute s)
    , countResource Geode
    , countBots Geode
    , countBots Obsidian
    , countResource Obsidian
    , countBots Clay
    , countResource Clay
    , countBots Ore
    , countResource Ore
    ]
  in tiebreakers funcs s1 s2