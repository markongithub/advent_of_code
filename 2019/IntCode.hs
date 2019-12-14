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
  -- IntCodeState m i _ _ = trace (show $ Map.elems (memoryMap state)) state
  IntCodeState m i _ _ = state
  nextOpCode = m!i -- this should not fail in these problems
  (actualOpCode, immediates) = parseOpCode nextOpCode
  in case actualOpCode of
    99 -> state
    1  -> processMathInstruction state (+) immediates
    2  -> processMathInstruction state (*) immediates
    3  -> processSetInstruction state
    4  -> processWriteInstruction state immediates
    _  -> error "Unexpected instruction code"

parameterCount :: Int -> Int
parameterCount opCode = case opCode of
  1 -> 3
  2 -> 3
  3 -> 1
  4 -> 1
  _ -> error "Unexpected opCode"

processSetInstruction :: IntCodeState -> IntCodeState
processSetInstruction (IntCodeState m i [] output) = error "set instruction with no input"
processSetInstruction (IntCodeState m i (inputHead:inputTail) output) = let
  newLocation = m!(i+1)
  newMemory = Map.insert newLocation inputHead m
  nextInstruction = i + 2
  in processNextInstruction (IntCodeState newMemory nextInstruction inputTail output)

processWriteInstruction :: IntCodeState -> [Bool] -> IntCodeState
processWriteInstruction state immediates = let
  (IntCodeState m i input output) = state
  [value] = getValues state (getParameters state 1) immediates
  newOutput = value:output
  nextInstruction = i + 2
  in processNextInstruction (IntCodeState m nextInstruction input newOutput)

processMathInstruction :: IntCodeState -> (Int -> Int -> Int) -> [Bool] ->
                          IntCodeState
processMathInstruction state op immediates = let
  m = memoryMap state
  params = getParameters state 3
  -- We only use immediate mode on our first two parameters.
  [x, y] = getValues state (take 2 params) (take 2 immediates)
  newLocation = params!!2
  result = x `op` y
  newMemory = Map.insert newLocation result m
  nextPointer = (instructionPointer state) + 4
  nextState = state { memoryMap = newMemory, instructionPointer = nextPointer }
  in processNextInstruction nextState

initialStateFromList :: [Int] -> [Int] -> IntCodeState
initialStateFromList l input = let
  tuples = zip [0..] l
  in IntCodeState (Map.fromList tuples) 0 input []

getValue :: IntCodeState -> (Int, Bool) -> Int
getValue (IntCodeState m _ _ _) (parameter,immediate) = case immediate of
  True -> parameter
  False -> m!parameter

getValues :: IntCodeState -> [Int] -> [Bool] -> [Int]
getValues state params immeds = map (getValue state) (zip params immeds)

getParameters :: IntCodeState -> Int -> [Int]
getParameters (IntCodeState m i _ _) count = let
  getParamAtIndex x = m!(i + x)
  in map getParamAtIndex [1..count]

processImmediateCodes0 :: Int -> Int -> [Bool] -> [Bool]
processImmediateCodes0 0 value accu = accu
processImmediateCodes0 count value accu = let
  leadingDigit = value `mod` 10
  remainingDigits = value `div` 10
  nextBool = case leadingDigit of
    1 -> True
    0 -> False
    _ -> error "Unexpected immediate digit"
  -- These are given to us backwards, but I am assembling the list backwards,
  -- so the list should go forwards?
  in processImmediateCodes0 (count - 1) remainingDigits (nextBool:accu)

parseOpCode :: Int -> (Int, [Bool])
parseOpCode opCode = let
  actualOpCode = opCode `mod` 100
  immediateDigits = opCode `div` 100
  expectedParameters = parameterCount actualOpCode
  immediateBools = processImmediateCodes0 expectedParameters immediateDigits []
  in (actualOpCode, immediateBools)
