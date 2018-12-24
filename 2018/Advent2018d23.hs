module Main where

import Data.List (maximumBy)

data Nanobot = Nanobot { x :: Int, y :: Int, z :: Int, radius :: Int }
                 deriving (Eq, Show)

distance :: Nanobot -> Nanobot -> Int
distance (Nanobot x1 y1 z1 _) (Nanobot x2 y2 z2 _) =
  (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2))

strongestBot :: [Nanobot] -> Nanobot
strongestBot ls = maximumBy (\b1 b2 -> compare (radius b1) (radius b2)) ls

parseNanobot :: String -> Nanobot
parseNanobot str = let
  afterLess = drop 5 str
  xStr = takeWhile (/= ',') afterLess
  afterX = drop 1 $ dropWhile (/= ',') afterLess
  yStr = takeWhile (/= ',') afterX
  afterY = drop 1 $ dropWhile (/= ',') afterX
  zStr = takeWhile (/= '>') afterY
  rStr = drop 5 $ dropWhile (/= '>') afterY
  [xInt, yInt, zInt, r] = map (\s -> (read s :: Int)) [xStr, yStr, zStr, rStr]
  in Nanobot xInt yInt zInt r

parseFile :: String -> IO [Nanobot]
parseFile f = let
  text = readFile f
  parseText s = map parseNanobot $ lines s
  in fmap parseText text
  
nanobotsInRange :: [Nanobot] -> Nanobot -> Int
nanobotsInRange ls bot = let
  filterFunc b2 = (distance bot b2) <= (radius bot)
  in length $ filter filterFunc ls

solvePart1 :: [Nanobot] -> Int
solvePart1 ls = let
  strongest = strongestBot ls
  in nanobotsInRange ls strongest


main :: IO ()
main = do
  testBots <- parseFile "input/Advent2018d23test.txt"
  putStrLn $ show $ solvePart1 testBots
  bots <- parseFile "input/Advent2018d23.txt"
  putStrLn $ show $ solvePart1 bots
