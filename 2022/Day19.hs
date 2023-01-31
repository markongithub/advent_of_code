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

buyRobot :: Blueprint -> Int -> State -> Resource -> State
buyRobot blueprint maxTime state robotType = let
  (State robots inventory oldGeodes t) = state
  price = blueprint!robotType
  subtractRes :: Inventory -> (Resource, Int) -> Inventory
  subtractRes someInv (res, q) = Map.adjust (\stupid -> stupid - q) res someInv
  newInventory = foldl subtractRes inventory $ Map.toList price
  newGeodes :: Int
  newGeodes = if (robotType == Geode) then (oldGeodes + (maxTime + 1) - t) else oldGeodes
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

spaces :: Int -> Int -> String
spaces depth maxTime = take (maxTime - depth) (repeat ' ')

evaluate :: Blueprint -> Int -> Int -> State -> Int -> ([State], Int)
evaluate bp maxTime depth state best = let
  State _ _ geodes time = state -- trace (spaces depth ++ show (depth, best, state)) state
  bailOut = maxGeodes2 bp maxTime state <= best
  options :: [State]
  options = availableOptions bp maxTime state
  options2 :: [State]
  options2 = if (null options) then error "null options" else options
  maxBySnd (a, x) (b, y) = if x > y then (a, x) else (b, y)
  foldEval :: ([State], Int) -> State -> ([State], Int)
  foldEval oldBest s = maxBySnd oldBest (evaluate bp maxTime (depth - 1) s (snd oldBest)) 
  (recurse1, recurse2) = foldl foldEval ([], best) options2
  tooLate = if (depth <= 0) then True else False
  in if (tooLate || time >= (maxTime + 1)) || bailOut
    then ([state], geodes)
    else (state:recurse1, recurse2)

evalToEnd :: Blueprint -> Int -> State -> ([State], Int)
evalToEnd bp maxTime state = let
  timeLeft = (maxTime + 2) - getCurMinute state
  in evaluate bp maxTime timeLeft state 0

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

canBuildPrice :: Blueprint -> Int -> State -> Resource -> Bool
canBuildPrice blueprint maxTime state res = let
  (State _ _ _ time) = state
  in case distanceToPrice blueprint state res of
    Nothing -> False
    Just t -> time + t < maxTime

waitAndBuyRobot :: Blueprint -> Int -> State -> Resource -> State
waitAndBuyRobot blueprint maxTime state res = let
  turnsToWait = fromJust $ distanceToPrice blueprint state res
  afterWaiting = head $ drop (turnsToWait + 1) $ iterate doMining state
  in buyRobot blueprint maxTime afterWaiting res

waitForEnd :: Int -> State -> State
waitForEnd maxTime state = let
  (State _ _ _ time) = state
  timeLeft = maxTime + 1 - time
  in head $ drop timeLeft $ iterate doMining state

availableOptions :: Blueprint -> Int -> State -> [State]
availableOptions blueprint maxTime oldState = let
  thingsToBuy = filter (canBuildPrice blueprint maxTime oldState) $ [Geode, Obsidian, Clay, Ore]
  giveUp = waitForEnd maxTime oldState
  afterBuying = map (waitAndBuyRobot blueprint maxTime oldState) thingsToBuy
  -- in if getCurMinute oldState >= 25 then [] else giveUp:afterBuying
  in if (getCurMinute oldState >= (maxTime + 1)) -- || (maxGeodes2 blueprint oldState < 12)
    then []
    else if null afterBuying
      then [giveUp]
      else afterBuying
--      else if Set.member Geode thingsToBuy
--        then [waitAndBuyRobot blueprint maxTime oldState Geode]
--        else afterBuying

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

maxGeodes2 :: Blueprint -> Int -> State -> Int
maxGeodes2 bp maxTime state = let
  (State r i g t) = state
  timeToClay = t + (minTimeToFirstRobot bp state Clay Ore)
  timeToObsidian = timeToClay + (minTimeToFirstRobot bp state Obsidian Clay)
  timeToGeode = timeToObsidian + (minTimeToFirstRobot bp state Geode Obsidian)
  geodeBotsICouldStillBuild = max 0 (maxTime - timeToGeode)
  in g + ((geodeBotsICouldStillBuild * (geodeBotsICouldStillBuild + 1)) `div` 2)

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

parseAndDebugQualityLevel :: Int -> String -> (Int, Int)
parseAndDebugQualityLevel maxTime str = let
  (idNum, bp) = parseLine str
  geodes = snd $ evaluateMemoSimple bp maxTime
  in (idNum, geodes)

testInput = [
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."
  , "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
  ]

solvePart1Pure :: [String] -> ([(Int, Int)], Int)
solvePart1Pure strs = let
  debugs = map (parseAndDebugQualityLevel 24) strs
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

solvePart2Pure :: [String] -> ([(Int, Int)], Int)
solvePart2Pure strs = let
  debugs = map (parseAndDebugQualityLevel 32) (take 3 strs)
  geodeYields = map snd debugs
  answer = product geodeYields
  in (debugs, answer)
  -- in sum qualityLevels
  -- return (sum qualityLevels)
  -- output = map parseAndDebugQualityLevel strs
  -- in output

solvePart2 = do
  text <- readFile "data/input19.txt"
  return $ solvePart2Pure $ lines text

applyPath0 :: Blueprint -> Int -> [Resource] -> State -> State
applyPath0 bp _ [] accu = accu
applyPath0 bp maxTime (r:rs) accu = let
  newAccu = waitAndBuyRobot bp maxTime accu r
  in applyPath0 bp maxTime rs newAccu

applyPath :: Blueprint -> Int -> [Resource] -> State
applyPath bp maxTime rs = applyPath0 bp maxTime rs initialState

-- myEdgeFunc :: Blueprint -> Int -> OutgoingEdgeFunc State
-- myEdgeFunc bp maxTime oldState = let
--  options = availableOptions bp maxTime oldState
--  geodeDiff newState = getGeodes oldState - getGeodes newState
--  makeEdge newState = (oldState, newState, geodeDiff newState)
--  in map makeEdge options

type StateCache = Map State Int
debugDepth = 99

evaluateMemo :: Blueprint -> Int -> Int -> Int -> StateCache -> State -> Int -> (StateCache, Int)
evaluateMemo bp maxTime depth cacheDepthMinutes cache state best = let
  State _ _ geodes time = if depth > debugDepth then (trace (spaces depth maxTime ++ show (depth, best, Map.size cache, state)) state) else state
  bailOut = maxGeodes2 bp maxTime state <= best
  options :: [State]
  options = availableOptions bp maxTime state
  options2 :: [State]
  options2 = if (null options) then error "null options" else options
  maxBySnd (a, x) (b, y) = if x > y then (a, x) else (b, y)
  foldEval :: (StateCache, Int) -> State -> (StateCache, Int)
  foldEval oldBest s = maxBySnd oldBest (evaluateMemo bp maxTime (depth - 1) cacheDepthMinutes (fst oldBest) s (snd oldBest)) 
  (newCache, value) = foldl foldEval (cache, best) options2
  finalCache = Map.insert state value newCache
  tooLate = if (depth <= 0) then True else False
  useCache = time <= cacheDepthMinutes
  answerFromCache = cache!state
  in if useCache && Map.member state cache
    then (cache, answerFromCache)
    else if (tooLate || time >= (maxTime + 1)) || bailOut
      then (cache, geodes)
      else (finalCache, value)

evaluateMemoSimple :: Blueprint -> Int -> (StateCache, Int)
evaluateMemoSimple bp maxTime = evaluateMemo bp maxTime maxTime 20 Map.empty initialState 0

tryCacheDepth :: Int -> Int
tryCacheDepth cacheDepth = snd $ evaluateMemo testBlueprint2 32 32 cacheDepth Map.empty initialState 0