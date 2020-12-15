module Day15 where

import Common
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Char (digitToInt)
import Debug.Trace (trace)

data GameState = GameState { startingNumbersAcc :: [Int]
                           , nextAgeAcc :: Int
                           , currentTurnAcc :: Int
                           , memoryAcc :: Map Int Int
                           } deriving (Eq, Show)

advanceGame :: GameState -> GameState
advanceGame (GameState starting next currentTurn memory) = let
  debugStr = ("turn " ++ show currentTurn ++ " speaking " ++ show next) --  ++ " map max " ++ show (maximum $ Map.elems memory))
  nextTurn = if (currentTurn `mod` 1000000 == 0) then (trace debugStr (currentTurn + 1)) else (currentTurn + 1)
  (spokenNumber, nextStarting) = case starting of
    []     -> (next, [])
    (x:xs) -> (x,xs)
  nextAge = case (Map.lookup spokenNumber memory) of
    Nothing -> 0
    Just t  -> (currentTurn - t)
  newMemory = Map.insert spokenNumber currentTurn memory
  nextState = GameState nextStarting nextAge nextTurn newMemory
  in nextState

nextSpoken :: GameState -> Int
nextSpoken (GameState starting age _ _) = case starting of
  []     -> age
  (x:xs) -> x

spokenNumbers :: GameState -> [Int]
spokenNumbers old = let
  next = nextSpoken old
  new = advanceGame old
  in (next:(spokenNumbers new))

initialState :: [Int] -> GameState
initialState starting = GameState starting undefined 1 Map.empty

parseInput :: String -> [Int]
parseInput s = let
  strings = splitOnCommas s
  ints = map (\s -> (read s :: Int)) strings
  in ints

spokenNumbersFromInput :: String -> [Int]
spokenNumbersFromInput input = let
  startingNumbers = parseInput input
  in spokenNumbers (initialState startingNumbers)

iterateQuiet :: Int -> (GameState -> GameState) -> GameState -> GameState
iterateQuiet 0 _ x = x
iterateQuiet n f x = let
  output = iterateQuiet (n - 1) f $! (f x)
  shouldTrace = n `mod` 1000000 == 0
  in if shouldTrace then (trace ("iterateQuiet: " ++ show (n, Map.size $ memoryAcc x)) output) else output

stateAfterNWords :: Int -> String -> GameState
stateAfterNWords n input = let
  startingNumbers = parseInput input
  in iterateQuiet n advanceGame (initialState startingNumbers)

nthWord :: Int -> String -> Int
nthWord n input = let
  finalState = stateAfterNWords (n-1) input
  in nextSpoken $ finalState

solvePart1Func :: String -> Int
solvePart1Func input = nthWord 2020 input

day15Input = "1,2,16,19,18,0"
solvePart1 :: Int
solvePart1 = solvePart1Func day15Input

part2Number = 30000000
solvePart2Func input = nthWord part2Number input
solvePart2 = solvePart2Func day15Input
