module Main where

import IntCode
import Data.Map ((!))
import qualified Data.Map as Map

runP1Example :: [Int] -> [Int]
runP1Example l = let
  IntCodeState finalMemory _ _ _ = processNextInstruction $ initialStateFromList l []
  in Map.elems finalMemory

solvePart1 :: [Int] -> [Int]
solvePart1 l = let
  IntCodeState finalMemory _ _ _ = processNextInstruction $ inputNounVerb 12 2 $ initialStateFromList l []
  in Map.elems finalMemory

inputNounVerb :: Int -> Int -> IntCodeState -> IntCodeState
inputNounVerb noun verb (IntCodeState m i input output) = let
  newMemory = Map.insert 2 verb $ Map.insert 1 noun m
  in IntCodeState newMemory i input output

tryPair :: IntCodeState -> (Int, Int) -> Int
tryPair state (noun, verb) = let
  (IntCodeState resultMemory _ _ _) = processNextInstruction $ inputNounVerb noun verb state
  in resultMemory!0

solvePart2 :: Int
solvePart2 = let
  initialState = initialStateFromList puzzleInput []
  allPairs = [ (noun,verb) | noun<-[0..99], verb<-[0..99] ]
  isSolution :: (Int, Int) -> Bool
  isSolution (noun, verb) = tryPair initialState (noun, verb) == 19690720
  (solutionN, solutionV) = head $ filter isSolution allPairs -- will crash if there isn't one
  in (100 * solutionN) + solutionV


example1 = [1,9,10,3,2,3,11,0,99,30,40,50]
example2 = [1,0,0,0,99]
example3 = [2,3,0,3,99]
example4 = [2,4,4,5,99,0]
example5 = [1,1,1,4,99,5,6,0,99]
puzzleInput = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,5,23,1,23,9,27,2,27,6,31,1,31,6,35,2,35,9,39,1,6,39,43,2,10,43,47,1,47,9,51,1,51,6,55,1,55,6,59,2,59,10,63,1,6,63,67,2,6,67,71,1,71,5,75,2,13,75,79,1,10,79,83,1,5,83,87,2,87,10,91,1,5,91,95,2,95,6,99,1,99,6,103,2,103,6,107,2,107,9,111,1,111,5,115,1,115,6,119,2,6,119,123,1,5,123,127,1,127,13,131,1,2,131,135,1,135,10,0,99,2,14,0,0]

main = do
  putStrLn $ show $ runP1Example example1
  putStrLn $ show $ runP1Example example2
  putStrLn $ show $ runP1Example example3
  putStrLn $ show $ runP1Example example4
  putStrLn $ show $ runP1Example example5
  putStrLn $ show $ solvePart1 puzzleInput
  putStrLn $ show $ solvePart2
