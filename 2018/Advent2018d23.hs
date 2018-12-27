module Main where

import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map

data Nanobot = Nanobot { x :: Int, y :: Int, z :: Int, radius :: Int }
                 deriving (Eq, Show)

distance :: (Int, Int, Int) -> Nanobot -> Int
distance (x1, y1, z1) (Nanobot x2 y2 z2 _) =
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
nanobotsInRange ls (Nanobot xC yC zC rC) = let
  filterFunc b2 = (distance (xC, yC, zC) b2) <= rC
  in length $ filter filterFunc ls

-- for every bot we are going to mark all the squares in its range
-- Map (Int, Int, Int) Int

type SquareMap = Map (Int, Int, Int) Int

squaresInRange :: Range3D -> [(Int, Int, Int)]
squaresInRange (xMin, xMax, yMin, yMax, zMin, zMax) =
  [(xR, yR, zR) | xR <- [xMin..xMax],
                  yR <- [yMin..yMax],
                  zR <- [zMin..zMax]]

inRangeOfBot :: (Int, Int, Int) -> Nanobot -> Bool
inRangeOfBot (x0, y0, z0) bot = let
  (Nanobot x1 y1 z1 r1) = bot
  in (distance (x0, y0, z0) bot) <= r1

inRangeOfBots :: [Nanobot] -> (Int, Int, Int) -> Int
inRangeOfBots bots coords = length $ filter (inRangeOfBot coords) bots

solvePart1 :: [Nanobot] -> Int
solvePart1 ls = let
  strongest = strongestBot ls
  in nanobotsInRange ls strongest

type Range3D = (Int, Int, Int, Int, Int, Int)

botRange :: [Nanobot] -> Range3D
botRange bots = let
  botRange0 [] xMin xMax yMin yMax zMin zMax =
    (xMin, xMax, yMin, yMax, zMin, zMax)
  botRange0 ((Nanobot x0 y0 z0 _):xs) xMin xMax yMin yMax zMin zMax = let
    newXMin = min xMin x0
    newXMax = max xMax x0
    newYMin = min yMin y0
    newYMax = max yMax y0
    newZMin = min zMin z0
    newZMax = max zMax z0
    in botRange0 xs newXMin newXMax newYMin newYMax newZMin newZMax
  (Nanobot x0 y0 z0 _) = head bots
  in botRange0 (tail bots) x0 x0 y0 y0 z0 z0

divideRange :: Int -> Int -> Int -> [Int]
divideRange minR maxD 0 = []
divideRange minR maxD divisions = let
  divisor = divisions + 1
  increment = max 1 ((maxD - minR) `div` divisor)
  nextStep = minR + increment
  recurse = nextStep : (divideRange nextStep maxD (divisions - 1))
  in if (minR == maxD) then [maxD] else recurse

solvePart2 :: [Nanobot] -> Int
solvePart2 bots = let
  allSquares :: [(Int, Int, Int)]
  allSquares = squaresInRange $ botRange bots
  allPairs = zip allSquares (map (inRangeOfBots bots) allSquares)
  maxPair = maximumBy (\b1 b2 -> compare (snd b1) (snd b2)) allPairs
  (c0, c1, c2) = fst maxPair
  in (abs c0 + abs c1 + abs c2)

main :: IO ()
main = do
  testBots <- parseFile "input/Advent2018d23test.txt"
  putStrLn $ show $ solvePart1 testBots
  bots <- parseFile "input/Advent2018d23.txt"
  putStrLn $ show $ solvePart1 bots
  testBots2 <- parseFile "input/Advent2018d23test2.txt"
  putStrLn $ show $ solvePart2 testBots2
  putStrLn $ show $ solvePart2 bots

