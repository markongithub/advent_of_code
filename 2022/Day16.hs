module Day16 where

import Common (splitAtSep)
import Data.List (sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Valve = Valve {
    getFlowRate :: Int
  , getExits :: [ValveID]
  , getIsOpen :: Bool
} deriving (Eq, Ord, Show)

type ValveID = String
data GameState = GameState {
    getMinutesLeft :: Int
  , getTotalRelease :: Int
  , getCurrentValve :: ValveID
  , getValveMap :: Map ValveID Valve
} deriving (Eq, Ord, Show)

moveToValve :: ValveID -> GameState -> GameState
moveToValve destination state = let
  (GameState _ _ current vMap) = state
  newState = state {
      getMinutesLeft = getMinutesLeft state - 1
    , getCurrentValve = destination
    }
  exits = getExits $ vMap!current
  allowed = any (== destination) exits
  in if allowed then newState else error "you can't get there from here"

openValve :: GameState -> GameState
openValve (GameState timeLeft total current map) = let
  valve = map!current
  totalToRelease = (getFlowRate valve) * timeLeft
  newValve = valve {getIsOpen = True}
  newMap = Map.insert current newValve map
  in GameState (timeLeft - 1) (total + totalToRelease) current newMap

lookupOrError :: Map ValveID Valve -> ValveID -> Valve
lookupOrError vMap vID = case Map.lookup vID vMap of
  Just v -> v
  Nothing -> error (vID ++ " is not in my keys " ++ (show $ Map.keys vMap))

possibleMoves :: GameState -> [GameState]
possibleMoves state = let
  (GameState _ _ current vMap) = state
  Valve _ exits alreadyOpen = lookupOrError vMap current
  openCurrent = openValve state -- ONLY IF IT IS CLOSED!
  travelMoves :: [GameState]
  travelMoves = map (\e -> moveToValve e state) exits
  in if alreadyOpen
       then travelMoves
       else openCurrent:travelMoves
  
evaluate :: Int -> Int -> GameState -> Int
evaluate _ _ (GameState 0 total _ _) = total
evaluate 0 _ (GameState _ total _ _) = total
evaluate recurseLevel alpha state = let
  choices = possibleMoves state
  newRecurseLevel = recurseLevel - 1
  sortedChoices = reverse $ sortOn theoreticalMax choices
  in recurseAcross newRecurseLevel alpha 0 sortedChoices

recurseAcross :: Int -> Int -> Int -> [GameState] -> Int
recurseAcross recurseLevel alpha bestSoFar [] = bestSoFar
recurseAcross recurseLevel alpha bestSoFar (x:xs) = let
  maxForThisOne = theoreticalMax x
  thisOne = evaluate recurseLevel alpha x
  newBest = max thisOne bestSoFar
  newAlpha = max thisOne alpha
  recurse = recurseAcross recurseLevel newAlpha newBest xs
  in if maxForThisOne < (max alpha bestSoFar) then newBest else recurse

theoreticalMax :: GameState -> Int
theoreticalMax state = let
  GameState timeLeft total current vMap = state
  closedValves = map getFlowRate $ reverse $ sortOn getFlowRate $ filter (not . getIsOpen) $ Map.elems vMap
  bestTimeStamps = takeWhile (>= 0) $ iterate (\x -> x - 2) timeLeft
  products :: [Int]
  products = zipWith (*) closedValves bestTimeStamps
  in total + sum products

trimLeadingSpace :: String -> String
trimLeadingSpace [] = []
trimLeadingSpace (x:xs) = if x == ' ' then xs else (x:xs)

parseValve :: String -> (ValveID, Valve)
parseValve s = let
  startOfID = drop (length "Valve ") s
  valveID = take 2 startOfID
  startOfRate = drop (length "XB has flow rate=") startOfID
  rateStr = takeWhile (/= ';') startOfRate
  rate = read rateStr :: Int
  startOfExits = drop (length rateStr + length "; tunnels lead to valve" + 1) startOfRate
  exitStrs = map trimLeadingSpace $ splitAtSep startOfExits ','
  in (valveID, Valve rate exitStrs False)

testInput = [
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
  , "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
  , "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
  , "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
  , "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
  , "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
  , "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
  , "Valve HH has flow rate=22; tunnel leads to valve GG"
  , "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
  , "Valve JJ has flow rate=21; tunnel leads to valve II"
  ]

testState :: GameState
testState = let
  pairs = map parseValve testInput
  vMap = Map.fromList pairs
  in GameState 29 0 "AA" vMap

applyList :: [a -> a] -> a -> a
applyList [] q = q
applyList (x:xs) q = applyList xs (x q)

examplePath :: [GameState -> GameState]
examplePath = let
  funcs = [
      moveToValve "DD"
    , openValve
    , moveToValve "CC"
    , moveToValve "BB"
    , openValve
    , moveToValve "AA"
    , moveToValve "II"
    , moveToValve "JJ"
    , openValve
    , moveToValve "II"
    , moveToValve "AA"
    , moveToValve "DD"
    , moveToValve "EE"
    , moveToValve "FF"
    , moveToValve "GG"
    , moveToValve "HH"
    , openValve
    , moveToValve "GG"
    , moveToValve "FF"
    , moveToValve "EE"
    ]
  in funcs

solvePart1Pure :: [String] -> Int
solvePart1Pure ls = let
  pairs = map parseValve ls
  vMap = Map.fromList pairs
  start = fst $ head pairs
  state = GameState 29 0 start vMap
  in evaluate 30 0 state

solvePart1 = let
  text = readFile "data/input16.txt"
  in fmap (solvePart1Pure . lines) text
  
