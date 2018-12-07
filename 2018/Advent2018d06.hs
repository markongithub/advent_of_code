module Main where

import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Map as Map (Map, adjust, delete, fromList, toList, member)

data Coords = Coords { x :: Int, y :: Int } deriving (Eq, Show)
type TargetID = Int
type Target = (TargetID, Coords)

totalPlane :: [Target] -> (Coords, Coords)
totalPlane ls = let
  lowerLeft = Coords (minimum (map (x . snd) ls)) (minimum (map (y . snd) ls))
  upperRight = Coords (maximum (map (x . snd) ls)) (maximum (map (y . snd) ls))
  in (lowerLeft, upperRight)

pairsInPlane :: (Coords, Coords) -> [Coords]
pairsInPlane (lowerLeft, upperRight) =
  [(Coords x0 y0) | x0 <- [ (x lowerLeft)..(x upperRight) ]
                  , y0 <- [ (y lowerLeft)..(y upperRight) ]]

-- for every square in the plane I need to calculate its distance from each
-- target

distance :: Coords -> Coords -> Int
distance (Coords x1 y1) (Coords x2 y2) = (abs (x1 - x2)) + (abs (y1 - y2))

sortBySnd :: (Ord b) => [(a, b)] -> [(a, b)]
sortBySnd ls =  sortBy (\(_, d1) (_, d2) -> compare d1 d2) ls

closestTarget :: [Target] -> Coords -> Maybe TargetID
closestTarget ls square = let
  distanceT square (tID, tCoords) = (tID, (distance square tCoords))
  distances = map (distanceT square) ls
  sorted = sortBySnd distances
  isATie = (snd (sorted!!0) == snd (sorted !!1))
  in if isATie then Nothing else Just (fst $ sorted!!0)

squareAssocs :: [Target] -> [Coords] -> [(Coords, Maybe TargetID)]
squareAssocs targets squares = let
  makeSquareEntry sq = (sq, closestTarget targets sq)
  in map makeSquareEntry squares

processTargets :: [Target] -> [(Coords, Maybe TargetID)]
processTargets targets = let
  -- I fail at life
  plane = totalPlane targets
  squares = pairsInPlane (totalPlane targets)
  in squareAssocs targets squares

parseCoords :: String -> Coords
parseCoords str = let
  -- I don't feel like installing a regex library
  firstHalf = takeWhile isDigit str
  secondHalf = drop 2 $ dropWhile isDigit str
  (x, y) = ((read firstHalf :: Int), (read secondHalf :: Int))
  in Coords x y

parseFile :: String -> IO [Target]
parseFile f = let
  text = readFile f
  parseText s = zip [0..] $ map parseCoords $ lines s
  in fmap parseText text

solvePart1File :: String -> IO Int
solvePart1File f = fmap solvePart1 (parseFile f)

solvePart1 :: [Target] -> Int
solvePart1 targets = let
  plane = totalPlane targets
  (Coords left bottom, Coords right top) = plane
  squares = pairsInPlane plane
  assocs = squareAssocs targets squares
  isOnBorder :: Coords -> Bool
  isOnBorder (Coords x y) = (x == left) || (x == right) ||
                            (y == bottom) || (y == top)
  -- so for each assoc, if the coords are on the border we have to somehow
  -- remember to disqualify that one
  -- let's use a Map of squares to area sizes, but delete keys if they're on
  -- the border
  initialMap :: Map.Map TargetID Int
  initialMap = Map.fromList $ zip (map fst targets) (repeat 0)
  handleAssoc :: Map.Map TargetID Int -> (Coords, Maybe TargetID)
                 -> Map.Map TargetID Int
  handleAssoc map (_, Nothing) = map
  handleAssoc map (c, Just t)
    | (isOnBorder c) = (Map.delete t map)
    | Map.member t map = Map.adjust (+ 1) t map
    | otherwise = map
  finalMap = foldl handleAssoc initialMap assocs
  areaSizesSorted = reverse $ sortBySnd $ Map.toList finalMap
  in snd $ head areaSizesSorted

solvePart2 :: Int -> [Target] -> Int
solvePart2 threshold targets = let
  plane = totalPlane targets
  (Coords left bottom, Coords right top) = plane
  squares = pairsInPlane plane
  distanceToAllTargets :: Coords -> Int
  distanceToAllTargets square = let
    distances = map (distance square) (map snd targets)
    in sum distances
  isInRegion square = (distanceToAllTargets square) < threshold
  in length $ (filter isInRegion squares) 

solvePart2File :: Int -> String -> IO Int
solvePart2File threshold f = fmap (solvePart2 threshold) (parseFile f)

main :: IO ()
main = do
  targets <- parseFile "input/Advent2018d06.txt"
  putStrLn $ show $ solvePart1 targets
  putStrLn $ show $ solvePart2 10000 targets
