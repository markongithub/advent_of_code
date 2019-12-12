module IntCode where

import Data.Map (Map, (!))
import qualified Data.Map as Map
-- import Debug.Trace (trace)

data IntCodeState = IntCodeState { memoryMap :: Map Int Int
                                 , instructionPointer :: Int }

processNextInstruction :: IntCodeState -> IntCodeState
processNextInstruction state = let
  IntCodeState m i = state
  -- nextInstruction = trace (show $ Map.elems m) m!i -- this should not fail in these problems
  nextInstruction = m!i -- this should not fail in these problems
  in case nextInstruction of
       99 -> state
       1  -> processMathInstruction state (+)
       2  -> processMathInstruction state (*)
       _  -> error "Unexpected instruction code"

processMathInstruction :: IntCodeState -> (Int -> Int -> Int) -> IntCodeState
processMathInstruction (IntCodeState m i) op = let
  result = (m!(m!(i+1))) `op` (m!(m!(i+2)))
  newLocation = m!(i+3)
  newMemory = Map.insert newLocation result m
  nextInstruction = i+4
  in processNextInstruction (IntCodeState newMemory nextInstruction)

initialStateFromList :: [Int] -> IntCodeState
initialStateFromList l = let
  tuples = zip [0..] l
  in IntCodeState (Map.fromList tuples) 0
