module Day02 where

data Move = Rock | Paper | Scissors deriving (Eq, Show)

instance Ord Move where
  Rock `compare` Paper = LT
  Rock `compare` Scissors = GT
  Paper `compare` Rock = GT
  Paper `compare` Scissors = LT
  Scissors `compare` Rock = LT
  Scissors `compare` Paper = GT

letterToMove :: Char -> Move
letterToMove 'A' = Rock
letterToMove 'X' = Rock
letterToMove 'B' = Paper
letterToMove 'Y' = Paper
letterToMove 'C' = Scissors
letterToMove 'Z' = Scissors
letterToMove _ = error "you fail it"

scoreByMove Rock = 1
scoreByMove Paper = 2
scoreByMove Scissors = 3

-- This is the outcome of m2 because the player's move comes second
playGame :: (Move, Move) -> Int
playGame (m2, m1)
  | m1 == m2 = 3 + scoreByMove m1
  | m1 > m2 = 6 + scoreByMove m1
  | otherwise = scoreByMove m1

parseRound :: String -> (Move, Move)
parseRound [c1, _, c2] = (letterToMove c1, letterToMove c2)

gameSum :: [String] -> Int
gameSum ls = sum $ map (playGame . parseRound) ls

solvePart1 :: IO Int
solvePart1 = do
  text <- readFile "data/input02.txt"
  return $ gameSum $ lines text

requiredMove :: (Move, Char) -> Move
requiredMove (m, 'Y') = m
requiredMove (Rock, 'Z') = Paper
requiredMove (Paper, 'Z') = Scissors
requiredMove (Scissors, 'Z') = Rock
requiredMove (Rock, 'X') = Scissors
requiredMove (Paper, 'X') = Rock
requiredMove (Scissors, 'X') = Paper

parseRound2 :: String -> (Move, Char)
parseRound2 [c1, _, c2] = (letterToMove c1, c2)

playPart2Round :: (Move,Char) -> Int
playPart2Round (m1, c) = playGame (m1, (requiredMove (m1, c)))

gameSum2 :: [String] -> Int
gameSum2 ls = sum $ map (playPart2Round . parseRound2) ls

solvePart2 :: IO Int
solvePart2 = do
  text <- readFile "data/input02.txt"
  return $ gameSum2 $ lines text