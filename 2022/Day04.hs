module Day04 where

splitAtSep :: String -> Char -> (String, String)
splitAtSep s sep = let
  firstPart = takeWhile (/= sep) s
  secondPart = drop (length firstPart + 1) s
  in (firstPart, secondPart)

intHyphenInt :: String -> (Int, Int)
intHyphenInt s = let
  (first, second) = splitAtSep s '-'
  in (read first :: Int, read second :: Int)

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine s = let
  (assignmentS1, assignmentS2) = splitAtSep s ','
  in (intHyphenInt assignmentS1, intHyphenInt assignmentS2)

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (a, b) (x, y) = (a <= x && b >=y) || (x <= a && y >=b)

solvePart1Line :: String -> Bool
solvePart1Line str = let
  (p1, p2) = parseLine str
  in fullyContains p1 p2

solvePart1 :: IO Int
solvePart1 = do
  text <- readFile "data/input05.txt"
  return $ length $ filter solvePart1Line $ lines text

overlapAtAll :: (Int, Int) -> (Int, Int) -> Bool
overlapAtAll (a, b) (x, y) = (a <= x && b >=x) || (a <= y && b >=y) || fullyContains (a, b) (x, y)

solvePart2Line :: String -> Bool
solvePart2Line str = let
  (p1, p2) = parseLine str
  in overlapAtAll p1 p2

solvePart2 :: IO Int
solvePart2 = do
  text <- readFile "data/input05.txt"
  return $ length $ filter solvePart2Line $ lines text

