module Day15 where

import Common
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)

data GameState = GameState { startingNumbersAcc :: [Int]
                           , nextSpokenAcc :: Int
                           , currentTurnAcc :: Int
                           , memoryAcc :: Map Int Int
                           } deriving (Eq, Show)

speakNextNumber :: GameState -> (Int, GameState)
speakNextNumber (GameState starting next currentTurn memory) = let
  (spokenNumber, nextStarting) = case starting of
    []     -> (next, [])
    (x:xs) -> (x,xs)
  nextSpoken = case (Map.lookup spokenNumber memory) of
    Nothing -> 0
    Just t  -> (currentTurn - t)
  newMemory = Map.insert spokenNumber currentTurn memory
  nextState = GameState nextStarting nextSpoken (currentTurn + 1) newMemory
  in (spokenNumber, nextState)
    
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

solvePart1Func :: String -> Int
solvePart1Func input = (spokenNumbersFromInput input)!!2019

day15Input = "1,2,16,19,18,0"
solvePart1 :: Int
solvePart1 = solvePart1Func day15Input
