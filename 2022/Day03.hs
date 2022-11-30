module Day03 where

import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set

letterPriority :: Char -> Int
letterPriority c = let
  ascii = Char.ord c
  in if ascii > 96 then (ascii - 96) else (ascii - (65-27))

splitLine :: String -> (String, String)
splitLine s = let
  halfLength = length s `div` 2
  in (take halfLength s, drop halfLength s)

sackIntersection :: (String, String) -> Char
sackIntersection (str1, str2) = let
  set1 = Set.fromList str1
  set2 = Set.fromList str2
  in Set.findMin $ Set.intersection set1 set2

linePriority :: String -> Int
linePriority = letterPriority . sackIntersection . splitLine

solvePart1 :: IO Int
solvePart1 = do
  text <- readFile "data/input03.txt"
  return $ sum $ map linePriority $ lines text

listsOf3 :: [a] -> [[a]]
listsOf3 [] = []
listsOf3 ls = let
--  first3 :: [a]
  first3 = take 3 ls
--  remainder :: [a]
  remainder = drop 3 ls
--  otherLists :: [[a]]
  otherLists = listsOf3 remainder
  in first3:otherLists

intersectionOfNLists :: Ord a => [[a]] -> a
intersectionOfNLists xs = Set.findMin $ intersectionOfNLists0 xs (Set.fromList $ head xs)

intersectionOfNLists0 :: Ord a => [[a]] -> Set a -> Set a
intersectionOfNLists0 [] accu = accu
intersectionOfNLists0 (x:xs) accu = let
  newAccu = Set.intersection accu (Set.fromList x)
  in intersectionOfNLists0 xs newAccu

threeSackPriority :: [String] -> Int
threeSackPriority = letterPriority . intersectionOfNLists

solvePart2Pure :: [String] -> Int
solvePart2Pure = sum . map threeSackPriority . listsOf3

solvePart2 :: IO Int
solvePart2 = do
  text <- readFile "data/input03.txt"
  return $ solvePart2Pure $ lines text
