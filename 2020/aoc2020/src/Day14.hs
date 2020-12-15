module Day14 where

import Data.Bits ((.&.), (.|.))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)

data MemoryState = MemoryState { andMaskAcc :: Int
                               , orMaskAcc :: Int
                               , mMapAcc :: Map Int Int
                               } deriving (Eq, Show)

replaceAll :: (Eq a) => [a] -> a -> a -> [a]
replaceAll [] _ _ = []
replaceAll (x:xs) old new = let
  newHead = if (x == old) then new else x
  in newHead:(replaceAll xs old new)

parseBinary0 :: String -> Int -> Int
parseBinary0 [] accu = accu
parseBinary0 (x:xs) accu = let
  newAccu = 2*accu + (digitToInt x)
  in parseBinary0 xs newAccu

parseBinary :: String -> Int
parseBinary s = parseBinary0 s 0

setMask :: String -> MemoryState -> MemoryState
setMask maskStr (MemoryState _ _ m) = let
  newAndMask = parseBinary $ replaceAll maskStr 'X' '1'
  newOrMask = parseBinary $ replaceAll maskStr 'X' '0'
  in MemoryState newAndMask newOrMask m

writeMemory :: Int -> Int -> MemoryState -> MemoryState
writeMemory address value (MemoryState andMask orMask oldMemory) = let
  newValue = (value .|. orMask) .&. andMask
  newMemory = Map.insert address newValue oldMemory
  in MemoryState andMask orMask newMemory

data Instruction = WriteMemory Int Int | SetMask String

parseMask :: String -> Instruction
parseMask s = SetMask (drop 7 s)

parseWrite :: String -> Instruction
parseWrite s = let
  upToAddr = drop 4 s
  addrStr = takeWhile (/= ']') upToAddr
  valueStr = drop (4 + length addrStr) upToAddr
  addr = (read addrStr :: Int)
  value = (read valueStr :: Int)
  in WriteMemory addr value

parseInstruction :: String -> Instruction
parseInstruction s = case (take 3 s) of
  "mem" -> parseWrite s
  "mas" -> parseMask s
  _     -> error "invalid instruction"

runInstruction :: MemoryState -> String -> MemoryState
runInstruction m s = let
  instruction = parseInstruction s
  in case instruction of
       WriteMemory a v -> writeMemory a v m
       SetMask v       -> setMask v m
initialState = MemoryState 0 0 Map.empty

solvePart1Pure :: [String] -> Int
solvePart1Pure instructions = let
  finalState = foldl runInstruction initialState instructions
  in sum $ Map.elems $ mMapAcc finalState

solvePart1 :: IO Int
solvePart1 = let
  input = fmap lines $ readFile "data/input14.txt"
  in fmap solvePart1Pure input

applyMaskC :: Char -> Char -> Char
applyMaskC address mask = case mask of
  '0' -> address
  '1' -> mask
  'X' -> mask
  _ -> error ("invalid bitmask character: " ++ show mask)

applyMask :: String -> String -> String
applyMask = zipWith applyMaskC

expandXes0 :: String -> [String] -> [Int]
expandXes0 [] accu = map (parseBinary . reverse) accu
expandXes0 (x:xs) accu = let
  expandedAccu = map ('0':) accu ++ map ('1':) accu
  simpleAccu = map (x:) accu
  newAccu = if x == 'X' then expandedAccu else simpleAccu
  in expandXes0 xs newAccu

expandXes :: String -> [Int]
expandXes s = expandXes0 s [""]

showBinary0 :: Int -> String -> String
showBinary0 0 accu = accu
showBinary0 i accu = let
  newInt = i `div` 2
  newCharacter = head $ show (i `mod` 2)
  newAccu = newCharacter:accu
  in showBinary0 newInt newAccu

showBinary :: Int -> String
showBinary i = showBinary0 i ""

showBinaryWithPadding :: Int -> Int -> String
showBinaryWithPadding i l = let
  withoutPadding = showBinary i
  paddingLength = l - (length withoutPadding)
  padding = take paddingLength (repeat '0')
  in padding ++ withoutPadding

maskAddresses :: Int -> String -> [Int]
maskAddresses addr mask = expandXes $ applyMask (showBinaryWithPadding addr (length mask)) mask

data Part2State = Part2State { maskAcc :: String
                             , mMapAcc2 :: Map Int Int
                             } deriving (Eq, Show)

writeMaskedAddr :: Int -> Int -> Part2State -> Part2State
writeMaskedAddr address value (Part2State mask oldMemory) = let
  addresses = maskAddresses address mask
  newPairs = zip addresses (repeat value)
  tempMap = Map.fromList newPairs
  newMemory = Map.union tempMap oldMemory
  in Part2State mask newMemory

initialState2 :: Part2State
initialState2 = Part2State "" Map.empty

runInstruction2 :: Part2State -> String -> Part2State
runInstruction2 m s = let
  instruction = parseInstruction s
  in case instruction of
       WriteMemory a v -> writeMaskedAddr a v m
       SetMask v       -> setMask2 v m

setMask2 :: String -> Part2State -> Part2State
setMask2 newMask (Part2State oldMask mMap) = Part2State newMask mMap

solvePart2Pure :: [String] -> Int
solvePart2Pure instructions = let
  finalState = foldl runInstruction2 initialState2 instructions
  in sum $ Map.elems $ mMapAcc2 finalState

solvePart2 :: IO Int
solvePart2 = let
  input = fmap lines $ readFile "data/input14.txt"
  in fmap solvePart2Pure input
