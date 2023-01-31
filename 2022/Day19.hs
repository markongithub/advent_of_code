module Day19 where

import Data.List (maximumBy, sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Common (splitAtSep)
import Data.Char (isDigit)
import Debug.Trace (trace)

data Resource = Ore | Clay | Obsidian | Geode deriving (Eq, Ord, Show)
type Inventory = Map Resource Int
data State = State {
    getRobots :: Inventory
  , getInventory :: Inventory
  , getGeodes :: Int
  , getCurMinute :: Int
  } deriving (Eq, Ord, Show)

type Price = Map Resource Int

type Blueprint = Map Resource Price

doMining :: State -> State
doMining (State robots inventory geodes time) = let
  updateInventory :: Inventory -> (Resource, Int) -> Inventory
  updateInventory imap (res, q) = Map.insertWith (+) res q imap
  finalInventory = foldl updateInventory inventory (Map.toList robots)
  in State robots finalInventory geodes (time + 1)

canBuyRobot :: Blueprint -> State -> Resource -> Bool
canBuyRobot blueprint (State _ inventory _ _) robotType = let
  price = blueprint!robotType
  canAfford (res, q) = (Map.findWithDefault 0 res inventory) >= q
  in all canAfford $ Map.toList price

buyRobot :: Blueprint -> State -> Resource -> State
buyRobot blueprint state robotType = let
  (State robots inventory oldGeodes t) = state
  price = blueprint!robotType
  subtractRes :: Inventory -> (Resource, Int) -> Inventory
  subtractRes someInv (res, q) = Map.adjust (\stupid -> stupid - q) res someInv
  newInventory = foldl subtractRes inventory $ Map.toList price
  newGeodes :: Int
  newGeodes = if (robotType == Geode) then (oldGeodes + 25 - t) else oldGeodes
  newRobots :: Inventory
  newRobots = if (robotType == Geode) then robots else Map.insertWith (+) robotType 1 robots
  in State newRobots newInventory newGeodes t
 
initialState = State (Map.singleton Ore 1) Map.empty 0 1

testBlueprint :: Blueprint
testBlueprint = Map.fromList [
    (Ore, Map.fromList [(Ore, 4)])
  , (Clay, Map.fromList [(Ore, 2)])
  , (Obsidian, Map.fromList [(Ore, 3), (Clay, 14)])
  , (Geode, Map.fromList [(Ore, 2), (Obsidian, 7)])
  ]

testBlueprint2 :: Blueprint
testBlueprint2 = Map.fromList [
    (Ore, Map.fromList [(Ore, 2)])
  , (Clay, Map.fromList [(Ore, 3)])
  , (Obsidian, Map.fromList [(Ore, 3), (Clay, 8)])
  , (Geode, Map.fromList [(Ore, 3), (Obsidian, 12)])
  ]

spaces :: Int -> String
spaces depth = take (15 - depth) (repeat ' ')

evaluate :: Blueprint -> Int -> State -> Int -> ([State], Int)
evaluate bp depth state best = let
  State _ _ geodes time = state -- trace (spaces depth ++ show (depth, best, state)) state
  bailOut = maxGeodes2 bp state <= best
  options :: [State]
  options = availableOptions bp state
  options2 :: [State]
  options2 = if (null options) then error "null options" else options
  maxBySnd (a, x) (b, y) = if x > y then (a, x) else (b, y)
  foldEval :: ([State], Int) -> State -> ([State], Int)
  foldEval oldBest s = maxBySnd oldBest (evaluate bp (depth - 1) s (snd oldBest)) 
  (recurse1, recurse2) = foldl foldEval ([], best) options2
  tooLate = if (depth <= 0) then True else False
  in if (tooLate || time >= 25) || bailOut
    then ([state], geodes)
    else (state:recurse1, recurse2)

evalToEnd :: Blueprint -> State -> ([State], Int)
evalToEnd bp state = let
  timeLeft = 26 - getCurMinute state
  in evaluate bp timeLeft state 0

-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
-- evalFold best state best

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
hasGeodeBot (State _ _ geodes _) = geodes > 0

shortestPathToGeode :: Blueprint -> Node -> ([Node], Distance)
shortestPathToGeode blueprint state = fromJust $ shortestPath blueprint state hasGeodeBot

distanceToPrice :: Blueprint -> State -> Resource -> Maybe Int
distanceToPrice blueprint (State robots inventory _ _) res = let
  price = blueprint!res
  necessaryResources = if (null $ Map.keys price) then error (show price) else Map.keys price
  deficit res = price!res - Map.findWithDefault 0 res inventory
  haveRobot res = Map.findWithDefault 0 res robots >= 1
  haveAllRobots = all haveRobot necessaryResources
  divideAndRoundUp x y = (x + y - 1) `div` y
  turnsToRes res = divideAndRoundUp (deficit res) (robots!res)
  maximumTurns = max 0 (maximum $ map turnsToRes necessaryResources)
  in if haveAllRobots then Just maximumTurns else Nothing

canBuildPrice :: Blueprint -> State -> Resource -> Bool
canBuildPrice blueprint state res = let
  (State _ _ _ time) = state
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
  (State _ _ _ time) = state
  timeLeft = 25 - time
  in head $ drop timeLeft $ iterate doMining state

availableOptions :: Blueprint -> State -> [State]
availableOptions blueprint oldState = let
  thingsToBuy = filter (canBuildPrice blueprint oldState) $ [Geode, Obsidian, Clay, Ore]
  giveUp = waitForEnd oldState
  afterBuying = map (waitAndBuyRobot blueprint oldState) thingsToBuy
  -- in if getCurMinute oldState >= 25 then [] else giveUp:afterBuying
  in if (getCurMinute oldState >= 25) -- || (maxGeodes2 blueprint oldState < 12)
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
  state3 = findNode blueprint state2 (\s -> getGeodes s >0)
  in (state2, state3)

testPath2 :: [Resource]
testPath2 = [
    Clay --3 
  ,  Clay --5
  ,  Clay -- 7
  ,  Obsidian --11 my paper example is here
  ,  Clay --12
  ,  Obsidian --15
  ,  Geode --18
  ,  Geode --21
  ]

after11 = applyPath testBlueprint $ take 4 testPath2
after12 = applyPath testBlueprint $ take 5 testPath2
after18 = applyPath testBlueprint $ take 7 testPath2

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
    , getGeodes
    , countBots Obsidian
    , countResource Obsidian
    , countBots Clay
    , countResource Clay
    , countBots Ore
    , countResource Ore
    ]
  in tiebreakers funcs s1 s2

distanceFromEachResource :: Blueprint -> State -> Resource -> [(Resource, Maybe Int)]
distanceFromEachResource blueprint (State robots inventory _ _) res = let
  price = blueprint!res
  necessaryResources = if (null $ Map.keys price) then error (show price) else Map.keys price
  deficit res = price!res - Map.findWithDefault 0 res inventory
  haveRobot res = Map.findWithDefault 0 res robots >= 1
  haveAllRobots = all haveRobot necessaryResources
  divideAndRoundUp x y = (x + y - 1) `div` y
  turnsToRes res = if (haveRobot res) then (res, Just (divideAndRoundUp (deficit res) (robots!res))) else (res, Nothing)
  in map turnsToRes necessaryResources

quadraticTime :: Int -> Int -> Int
quadraticTime baseRate target = let
  -- a = 0.5 so 2a = 1 and 4a=2 so the math is easy
  b = 0.5 + fromIntegral baseRate
  c = -1.0 * fromIntegral target
  sqrtTerm = sqrt((b * b) - (2 * c))
  in ceiling (sqrtTerm - b)

minTimeToFirstRobot :: Blueprint -> State -> Resource -> Resource -> Int
minTimeToFirstRobot bp (State robots inv g t) targetRes sourceRes = let
  alreadyHaveIt = Map.findWithDefault 0 targetRes robots > 0
  price = (bp!targetRes)!sourceRes
  needed = max 0 (price - (Map.findWithDefault 0 sourceRes inv))
  timeToGetIt = quadraticTime (Map.findWithDefault 0 sourceRes robots) needed
  in if alreadyHaveIt then 0 else timeToGetIt

maxGeodes :: State -> Int
maxGeodes (State r i g t) = let
  geodeBotsICouldStillBuild = 24 - t
  in g + ((geodeBotsICouldStillBuild * (geodeBotsICouldStillBuild + 1)) `div` 2)

maxGeodes2 :: Blueprint -> State -> Int
maxGeodes2 bp state = let
  (State r i g t) = state
  timeToClay = t + (minTimeToFirstRobot bp state Clay Ore)
  timeToObsidian = timeToClay + (minTimeToFirstRobot bp state Obsidian Clay)
  timeToGeode = timeToObsidian + (minTimeToFirstRobot bp state Geode Obsidian)
  geodeBotsICouldStillBuild = max 0 (24 - timeToGeode)
  in g + ((geodeBotsICouldStillBuild * (geodeBotsICouldStillBuild + 1)) `div` 2)

evaluateSimple :: Blueprint -> Int
evaluateSimple bp = snd $ evaluate bp (length testPath2 + 3) initialState 0

qualityLevel :: (Int, Blueprint) -> Int
qualityLevel (bpID, bp) = let
  geodes = snd $ evalToEnd bp initialState
  in bpID * geodes

parseLine :: String -> (Int, Blueprint)
parseLine str = let
  startOfID = drop (length "Blueprint ") str
  digits = takeWhile isDigit startOfID
  idInt :: Int
  idInt = read digits
  startOfCosts = drop 2 $ dropWhile (/= ':') startOfID
  costs = splitAtSep startOfCosts '.'
  blueprint = Map.fromList $ map parseCost costs
  in (idInt, blueprint)

stringToResource :: String -> Resource
stringToResource str = case str of 
  "ore" -> Ore
  "clay" -> Clay
  "obsidian" -> Obsidian
  "geode" -> Geode
  whatever -> error ("I don't know how to parse \"" ++ whatever ++ "\"")

parseCost :: String -> (Resource, Price)
parseCost str = let
  startOfResourceStr = drop 2 $ dropWhile (/= 'h') str
  resourceStr = takeWhile (/= ' ') startOfResourceStr
  startOfPrice = drop (length resourceStr + length " robot costs ") startOfResourceStr
  price = Map.fromList $ parsePrice startOfPrice
  in (stringToResource resourceStr, price)

parsePrice :: String -> [(Resource, Int)]
parsePrice [] = []
parsePrice str = let 
  -- 2 ore and 20 clay
  digitsStr = takeWhile isDigit str
  value :: Int
  value = read digitsStr
  startOfResourceStr = drop (length digitsStr + 1) str
  resourceStr = takeWhile (/= ' ') startOfResourceStr
  remainder = dropWhile (not . isDigit) $ drop (length digitsStr + 1 + length resourceStr) str
  resource = stringToResource resourceStr
  in (resource, value):(parsePrice remainder)

parseAndGetQualityLevel :: String -> Int
parseAndGetQualityLevel str = qualityLevel $ parseLine str

parseAndDebugQualityLevel :: String -> (Int, Int)
parseAndDebugQualityLevel str = let
  (idNum, bp) = parseLine str
  geodes = snd $ evalToEnd bp initialState
  in (idNum, geodes)

testInput = [
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
  , "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
  ]

solvePart1Pure :: [String] -> ([(Int, Int)], Int)
solvePart1Pure strs = let
  debugs = map parseAndDebugQualityLevel strs
  qualityLevels = map (\(x, y) -> x * y) debugs
  answer = sum qualityLevels
  in (debugs, answer)
  -- in sum qualityLevels
  -- return (sum qualityLevels)
  -- output = map parseAndDebugQualityLevel strs
  -- in output

solvePart1 = do
  text <- readFile "data/input19.txt"
  return $ solvePart1Pure $ lines text

buy2Ores = waitAndBuyRobot testBlueprint2 initialState Ore
buy3Ores = waitAndBuyRobot testBlueprint2 buy2Ores Ore
thenClay = waitAndBuyRobot testBlueprint2 buy3Ores Clay
thenAnotherClay = waitAndBuyRobot testBlueprint2 thenClay Clay
thirdClay = waitAndBuyRobot testBlueprint2 thenAnotherClay Clay
fourthClay = waitAndBuyRobot testBlueprint2 thirdClay Clay

applyPath0 :: Blueprint -> [Resource] -> State -> State
applyPath0 bp [] accu = accu
applyPath0 bp (r:rs) accu = let
  newAccu = waitAndBuyRobot bp accu r
  in applyPath0 bp rs newAccu

applyPath :: Blueprint -> [Resource] -> State
applyPath bp rs = applyPath0 bp rs initialState

-- 806 is my part 1 solution
-- bptest2 does not get you to 10

childrenWithScores :: Blueprint -> Int -> State -> Int -> [(State, Int)]
childrenWithScores bp depth state best = let
  State _ _ geodes time = state -- trace (spaces depth ++ show (depth, state)) state
  bailOut = maxGeodes2 bp state <= best
  options :: [State]
  options = availableOptions bp state
  options2 :: [State]
  options2 = if (null options) then error "null options" else options
  pairWithScore s = (s, snd $ evaluate bp (depth - 1) s best)
  in map pairWithScore options2