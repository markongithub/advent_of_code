module Main where

import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Prerequisites = Map Char (Set Char)

parseFile :: String -> IO Prerequisites
parseFile f = let
  prereq str = head $ drop 5 str
  -- Step R must be finished before step Z can begin.
  -- 1234567890123456789012345678901234567890
  target str = head $ drop 36 str
  updateMap :: Prerequisites -> String -> Prerequisites
  updateMap map str = let
    curSet = Map.findWithDefault Set.empty (target str) map
    newMap0 = Map.insert (target str) (Set.insert (prereq str) curSet) map
    newMap1 = if (Map.member (prereq str) map) then newMap0 else Map.insert (prereq str) Set.empty newMap0
    in if (length str > 36) then newMap1 else (error str)
  text = readFile f
  parseText str = foldl updateMap Map.empty (lines str)
  in fmap parseText text

nextAvailableJob :: Prerequisites -> (Set Char) -> Maybe Char
nextAvailableJob map inProgress = let
  isReady :: Prerequisites -> (Char, Set Char) -> Bool
  unavailable = (Set.union inProgress $ Map.keysSet map)
  isReady map (_, cPrereqs) =
    -- this version doesn't have Set.disjoint!
    Set.null $ Set.intersection cPrereqs unavailable
  candidates = filter (isReady map) $ Map.toList map
  in if (null candidates) then Nothing else Just (fst $ head candidates)

solvePart1 :: Prerequisites -> String
solvePart1 initialMap = let
  buildString :: Prerequisites -> String -> String
  buildString map output
    | Map.null map = reverse output
    | otherwise = let
        nextLetter = fromJust (nextAvailableJob map Set.empty)
        newOutput = nextLetter:output
        newMap = Map.delete nextLetter map
        in buildString newMap newOutput
  in buildString initialMap ""

data WorkerState = Idle | Working { assignment :: Char, secondsLeft :: Int}
                   deriving (Eq, Show)
type WorkerID = Int
data JobState = JobState { taskMap :: Prerequisites
                         , workerMap :: Map WorkerID WorkerState
                         , timestamp :: Int } deriving (Show)

taskDuration :: Char -> Int
taskDuration c = (ord c) - 64

tasksInProgress :: JobState -> Set Char
tasksInProgress state = let
  getAssignment Idle = error "You were supposed to filter these out"
  getAssignment (Working t _) = t
  working = filter (/= Idle) $ Map.elems (workerMap state)
  in Set.fromList $ map getAssignment $ working

tryToFindTask :: JobState -> WorkerID -> Int -> JobState
tryToFindTask state worker cost = let
  (JobState tasks workers ts) = state
  task = nextAvailableJob tasks (tasksInProgress state)
  in case task of
    Nothing -> JobState tasks (Map.insert worker Idle workers) ts
    Just t -> JobState (Map.delete t tasks)
                (Map.insert worker (Working t ((taskDuration t) + cost))
                  workers) ts

pickWorkersNextMove :: Int -> JobState -> WorkerID -> JobState
pickWorkersNextMove cost state worker = let
  JobState taskMap workerMap timestamp = state
  in case (workerMap!worker) of
      Idle -> tryToFindTask state worker cost
      Working _ _ -> state

assignWorkers state cost = foldl (pickWorkersNextMove cost) state
                             (Map.keys $ workerMap state)

advanceTask :: JobState -> WorkerID -> JobState
advanceTask state worker = let
  wMap = workerMap state
  newWorkerState = case (wMap!worker) of
                     Idle -> Idle
                     Working t 1 -> Idle
                     Working t n -> Working t (n - 1)
  newWorkerMap = Map.insert worker newWorkerState wMap
  in state { workerMap = newWorkerMap }

advanceTime cost state = let
  newState = (assignWorkers state cost) { timestamp = (timestamp state) + 1 }
  in foldl advanceTask newState (Map.keys $ workerMap newState)

isFinished state = Map.null (taskMap state) && Set.null (tasksInProgress state)

solvePart2 :: Prerequisites -> Int -> Int -> Int
solvePart2 prereqs workerCount cost = let
  workers = Map.fromList $ zip [1..workerCount] (repeat Idle)
  initialState = JobState prereqs workers 0
  recurse state
    | isFinished state = timestamp state
    | otherwise = recurse $ advanceTime cost state
  in recurse initialState
 
main :: IO ()
main = do
  testData <- parseFile "input/Advent2018d07-test.txt"
  putStrLn $ show $ solvePart1 testData
  prereqs <- parseFile "input/Advent2018d07.txt"
  putStrLn $ show $ solvePart1 prereqs
  putStrLn $ show $ solvePart2 testData 2 0
  putStrLn $ show $ solvePart2 prereqs 5 60
