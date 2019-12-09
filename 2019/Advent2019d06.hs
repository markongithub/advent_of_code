module Main where

-- import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map

type OrbitMap = Map String String

findOrbits :: OrbitMap -> String -> Int
findOrbits om key = case Map.lookup key om of
  Nothing -> 0
  Just directOrbit -> 1 + (findOrbits om directOrbit)

example1 = Map.fromList [ ("B","COM"), ("C","B"), ("D","C"), ("E","D")
                        , ("F","E"), ("G","B"), ("H","G"), ("I","D"), ("J","E")
                        , ("K","J"), ("L","K") ]

parseLine :: String -> (String, String)
parseLine str = let
  orbitee = takeWhile (/= ')') str
  orbiter = drop (length orbitee + 1) str
  in (orbiter, orbitee)

parseText :: String -> OrbitMap
parseText str = Map.fromList $ map parseLine $ lines str

countAllOrbits :: OrbitMap -> Int
countAllOrbits om = sum $ map (findOrbits om) $ Map.keys om

main = do
  putStrLn $ show $ findOrbits example1 "D"
  putStrLn $ show $ findOrbits example1 "L"
  example_text <- readFile "input/Advent2019d06_example.txt"
  putStrLn $ show (parseText example_text == example1)
  putStrLn $ show $ countAllOrbits example1
  input_text <- readFile "input/Advent2019d06_input.txt"
  putStrLn $ show $ countAllOrbits $ parseText input_text

