module Day01 where

import Data.Char (digitToInt, isDigit)

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs [x] = []
allPairs (x:xs) = let
  withX = zip (repeat x) xs
  withoutX = allPairs xs
  in withX ++ withoutX

sumsTo2020 :: (Int,Int) -> Bool
sumsTo2020 (x,y) = x + y == 2020

productOf2020Sums :: [Int] -> Int
productOf2020Sums xs = let
  (x,y) = head $ filter sumsTo2020 $ allPairs xs
  in x * y

parseFile :: String -> IO [Int]
parseFile f = let
  strs = readFile f
  parseInts xs = map (\s -> read s :: Int) $ lines xs
  in fmap parseInts strs

solvePart1 :: IO Int
solvePart1 = let
  input = parseFile "data/input01.txt"
  in fmap productOf2020Sums input

allTriples :: [a] -> [(a,a,a)]
allTriples [a,b,c] = [(a,b,c)]
allTriples (x:xs) = let
  withX = map (\(y,z) -> (x,y,z)) $ allPairs xs
  withoutX = allTriples xs
  in withX ++ withoutX

productOf2020TripleSum :: [Int] -> Int
productOf2020TripleSum xs = let
  sumsTo2020Triple (x,y,z) = x + y + z == 2020
  (x,y,z) = head $ filter sumsTo2020Triple $ allTriples xs
  in x * y * z

solvePart2 :: IO Int
solvePart2 = let
  input = parseFile "data/input01.txt"
  in fmap productOf2020TripleSum input


