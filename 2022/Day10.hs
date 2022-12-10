module Day10 where

import Data.List (intercalate)

data Instruction = NoOp | AddX Int
                   deriving (Eq, Ord, Show)

data CPUState = CPUState {
    getCycle :: Int
  , getXRegister :: Int
} deriving (Eq, Ord, Show)

parseInstruction :: String -> Instruction
parseInstruction str = case (take 4 str) of
  "noop" -> NoOp
  "addx" -> AddX (read (drop 5 str) :: Int)

testInput = map parseInstruction [
    "addx 15"
  , "addx -11"
  , "addx 6"
  , "addx -3"
  , "addx 5"
  , "addx -1"
  , "addx -8"
  , "addx 13"
  , "addx 4"
  , "noop"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx 5"
  , "addx -1"
  , "addx -35"
  , "addx 1"
  , "addx 24"
  , "addx -19"
  , "addx 1"
  , "addx 16"
  , "addx -11"
  , "noop"
  , "noop"
  , "addx 21"
  , "addx -15"
  , "noop"
  , "noop"
  , "addx -3"
  , "addx 9"
  , "addx 1"
  , "addx -3"
  , "addx 8"
  , "addx 1"
  , "addx 5"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx -36"
  , "noop"
  , "addx 1"
  , "addx 7"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "addx 6"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx 7"
  , "addx 1"
  , "noop"
  , "addx -13"
  , "addx 13"
  , "addx 7"
  , "noop"
  , "addx 1"
  , "addx -33"
  , "noop"
  , "noop"
  , "noop"
  , "addx 2"
  , "noop"
  , "noop"
  , "noop"
  , "addx 8"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx 17"
  , "addx -9"
  , "addx 1"
  , "addx 1"
  , "addx -3"
  , "addx 11"
  , "noop"
  , "noop"
  , "addx 1"
  , "noop"
  , "addx 1"
  , "noop"
  , "noop"
  , "addx -13"
  , "addx -19"
  , "addx 1"
  , "addx 3"
  , "addx 26"
  , "addx -30"
  , "addx 12"
  , "addx -1"
  , "addx 3"
  , "addx 1"
  , "noop"
  , "noop"
  , "noop"
  , "addx -9"
  , "addx 18"
  , "addx 1"
  , "addx 2"
  , "noop"
  , "noop"
  , "addx 9"
  , "noop"
  , "noop"
  , "noop"
  , "addx -1"
  , "addx 2"
  , "addx -37"
  , "addx 1"
  , "addx 3"
  , "noop"
  , "addx 15"
  , "addx -21"
  , "addx 22"
  , "addx -6"
  , "addx 1"
  , "noop"
  , "addx 2"
  , "addx 1"
  , "noop"
  , "addx -10"
  , "noop"
  , "noop"
  , "addx 20"
  , "addx 1"
  , "addx 2"
  , "addx 2"
  , "addx -6"
  , "addx -11"
  , "noop"
  , "noop"
  , "noop"
  ]

runInstruction :: CPUState -> Instruction -> [CPUState]
runInstruction (CPUState cycle x) ins = case ins of
  NoOp -> [CPUState (cycle + 1) x]
  AddX i -> [CPUState (cycle + 1) x, CPUState (cycle + 2) (x + i)]

runInstructions :: CPUState -> [Instruction] -> [CPUState]
runInstructions state [] = []
runInstructions state (x:xs) = let
  fromFirst = runInstruction state x
  in fromFirst ++ (runInstructions (last fromFirst) xs)

initialState = CPUState 1 1

strength :: CPUState -> Int
strength (CPUState cycle x) = cycle * x

statesEveryNPlusM :: Int -> Int-> [CPUState] -> [CPUState]
statesEveryNPlusM interval remainder ls = let
  isMultiple s = getCycle s `mod` interval == remainder
  in filter isMultiple ls

solvePart1Pure :: [Instruction] -> Int
solvePart1Pure ls = sum $ map strength $ statesEveryNPlusM 40 20 $ runInstructions initialState ls

solvePart1 :: IO Int
solvePart1 = fmap (solvePart1Pure . map parseInstruction . lines) $ readFile "data/input10.txt"

charForCycle :: CPUState -> Char
charForCycle (CPUState cycle x) = let
  hPos = (cycle - 1) `mod` 40
  in if abs (x - hPos) <= 1 then '#' else '.'

splitEvery :: Int -> [a] -> [[a]]
splitEvery i [] = []
splitEvery i xs = (take i xs):(splitEvery i (drop i xs))

showScreen :: String -> IO ()
showScreen s = putStrLn $ intercalate "\n" $ splitEvery 40 s

solvePart2 :: IO ()
solvePart2 = do
  text <- readFile "data/input10.txt"
  let output = map charForCycle $ initialState:(runInstructions initialState $ map parseInstruction $ lines text)
  showScreen output