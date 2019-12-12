module IntCode where

import Data.Map (Map, (!))
import qualified Data.Map as Map
-- import Debug.Trace (trace)

data IntCodeState = IntCodeState { memoryMap :: Map Int Int
                                 , instructionPointer :: Int
                                 , inputRemaining :: [Int]
                                 , outputStream :: [Int] }

processNextInstruction :: IntCodeState -> IntCodeState
processNextInstruction state = let
  IntCodeState m i _ _ = state
  -- nextInstruction = trace (show $ Map.elems m) m!i -- this should not fail in these problems
  nextInstruction = m!i -- this should not fail in these problems
  in case nextInstruction of
       99 -> state
       1  -> processMathInstruction state (+)
       2  -> processMathInstruction state (*)
       3  -> processSetInstruction state
       4  -> processWriteInstruction state
       _  -> error "Unexpected instruction code"

processSetInstruction :: IntCodeState -> IntCodeState
processSetInstruction (IntCodeState m i (inputHead:inputTail) output) = let
  newLocation = m!(i+1)
  newMemory = Map.insert newLocation inputHead m
  nextInstruction = i + 2
  in processNextInstruction (IntCodeState newMemory nextInstruction inputTail output)

processWriteInstruction :: IntCodeState -> IntCodeState
processWriteInstruction (IntCodeState m i input output) = let
  location = m!(i+1)
  value = m!location
  newOutput = value:output
  nextInstruction = i + 2
  in processNextInstruction (IntCodeState m nextInstruction input newOutput)

processMathInstruction :: IntCodeState -> (Int -> Int -> Int) -> IntCodeState
processMathInstruction (IntCodeState m i input output) op = let
  result = (m!(m!(i+1))) `op` (m!(m!(i+2)))
  newLocation = m!(i+3)
  newMemory = Map.insert newLocation result m
  nextInstruction = i+4
  in processNextInstruction (IntCodeState newMemory nextInstruction input output)

initialStateFromList :: [Int] -> [Int] -> IntCodeState
initialStateFromList l input = let
  tuples = zip [0..] l
  in IntCodeState (Map.fromList tuples) 0 input []
