module Day15 where

import Common
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Debug.Trace (trace)

data GameState = GameState { startingNumbersAcc :: [Int]
                           , nextSpokenAcc :: Int
                           , currentTurnAcc :: Int
                           , memoryAcc :: Map Int Int
                           } deriving (Eq, Show)

speakNextNumber :: GameState -> (Int, GameState)
speakNextNumber (GameState starting next currentTurn memory) = let
  debugStr = ("turn " ++ show currentTurn ++ " map size " ++ show (Map.size memory))
  nextTurn = (currentTurn + 1) -- if (currentTurn `mod` 10000 == 0) then (trace debugStr (currentTurn + 1)) else (currentTurn + 1)
  (spokenNumber, nextStarting) = case starting of
    []     -> (next, [])
    (x:xs) -> (x,xs)
  nextSpoken = case (Map.lookup spokenNumber memory) of
    Nothing -> 0
    Just t  -> (currentTurn - t)
  newMemory = Map.insert spokenNumber currentTurn memory
  nextState = GameState nextStarting nextSpoken nextTurn newMemory
  in (spokenNumber, nextState)

silentTurn :: GameState -> GameState
silentTurn state = snd $ speakNextNumber state
    
spokenNumbers :: GameState -> [Int]
spokenNumbers old = let
  (next, new) = speakNextNumber old
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

stateAfterNWords :: Int -> String -> GameState
stateAfterNWords n input = let
  startingNumbers = parseInput input
  in head $ drop (n-1) $ iterate silentTurn (initialState startingNumbers)

nthWord :: Int -> String -> Int
nthWord n input = let
  finalState = stateAfterNWords n input
  in nextSpokenAcc $ finalState

solvePart1Func :: String -> Int
solvePart1Func input = nthWord 2020 input

day15Input = "1,2,16,19,18,0"
solvePart1 :: Int
solvePart1 = solvePart1Func day15Input

solvePart2Func input = nthWord 30000000 input
solvePart2 = solvePart2Func day15Input
