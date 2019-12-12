module IntCode where

import Data.Map (Map, (!))
import qualified Data.Map as Map
-- import Debug.Trace (trace)

data IntCodeState = IntCodeState { memoryMap :: Map Int Int
                                 , current :: Int }

processNextInstruction :: IntCodeState -> IntCodeState
processNextInstruction state = let
  IntCodeState m c = state
  -- nextInstruction = trace (show $ Map.elems m) m!c -- this should not fail in these problems
  nextInstruction = m!c -- this should not fail in these problems
  in case nextInstruction of
       99 -> state
       1  -> processMathInstruction state (+)
       2  -> processMathInstruction state (*)
       _  -> error "Unexpected instruction code"

processMathInstruction :: IntCodeState -> (Int -> Int -> Int) -> IntCodeState
processMathInstruction (IntCodeState m c) op = let
  result = (m!(m!(c+1))) `op` (m!(m!(c+2)))
  newLocation = m!(c+3)
  newMemory = Map.insert newLocation result m
  nextInstruction = c+4
  in processNextInstruction (IntCodeState newMemory nextInstruction)

initialStateFromList :: [Int] -> IntCodeState
initialStateFromList l = let
  tuples = zip [0..] l
  in IntCodeState (Map.fromList tuples) 0
