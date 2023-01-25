module Day15 where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

type Coords = (Int, Int)
type ClosestBeacon = (Coords, Coords)
parseLine :: String -> ClosestBeacon
parseLine s = let
  startOfX = drop (length "Sensor at x=") s
  sensorXStr = takeWhile (/= ',') startOfX
  startOfY = drop (length sensorXStr + 4) startOfX
  sensorYStr = takeWhile (/= ':') startOfY
  startOfBX = drop (length sensorYStr + length ": closest beacon is at x=") startOfY
  beaconXStr = takeWhile (/= ',') startOfBX
  beaconYStr = drop (length beaconXStr + 4) startOfBX
  in ((read sensorXStr :: Int, read sensorYStr :: Int),
      (read beaconXStr :: Int, read beaconYStr :: Int))

testInput = [
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
  , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
  , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
  , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
  , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
  , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
  , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
  , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
  , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
  , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
  , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
  , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
  , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
  , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
  ]

positionsWithoutBeaconInRow :: ClosestBeacon -> Int -> Int -> Int -> [Coords]
positionsWithoutBeaconInRow ((sx, sy), (bx, by)) row minCoord maxCoord = let
  mDistance = abs (sx - bx) + abs (sy - by)
  rowDistanceFromSensor = abs (row - sy)
  remainingDistance = mDistance - rowDistanceFromSensor
  allowedColumns :: [Int]
  allowedColumns = if remainingDistance < 0
                     then []
                     else [(max minCoord (sx - remainingDistance))..(min maxCoord (sx + remainingDistance))]
  matchRowWithColumn :: Int -> Coords
  matchRowWithColumn c = (c, row)
  in map matchRowWithColumn allowedColumns

allEmptyPositionsInRow :: [ClosestBeacon] -> Int -> Int -> Int -> Set Coords
allEmptyPositionsInRow beacons row minCoord maxCoord = let
  findPositions beacon = positionsWithoutBeaconInRow beacon row minCoord maxCoord
  listOfLists = map findPositions beacons
  listOfSets = map Set.fromList listOfLists
  allCoords = Set.unions listOfSets
  in allCoords

beaconPossibilities :: [ClosestBeacon] -> Int -> Int -> Int -> Set Coords
beaconPossibilities beacons row minCoord maxCoord = let
  beaconsOnly = Set.fromList $ map snd beacons
  emptyCoords = allEmptyPositionsInRow beacons row minCoord maxCoord
  in Set.difference emptyCoords beaconsOnly

solvePart1Pure :: [String] -> Int -> Int
solvePart1Pure input row = let
  beacons = map parseLine input
  in Set.size $ beaconPossibilities beacons row minBound maxBound

solvePart1 = do
  text <- readFile "data/input15.txt"
  return $ solvePart1Pure (lines text) 2000000

findBeacon :: [ClosestBeacon] -> Int -> Int -> Coords
findBeacon beacons minCoord maxCoord = let
  checkRow row = allEmptyPositionsInRow beacons row minCoord maxCoord
  fullRow row = Set.fromList $ zip [minCoord..maxCoord] (repeat row)
  complement row = Set.difference (fullRow row) (checkRow row)
  allResults = map checkRow [minCoord..maxCoord]
  interestingResults = filter (\s -> Set.size s < (maxCoord - minCoord + 1)) allResults
  interestingRow = snd $ Set.findMin $ head interestingResults
  in Set.findMin $ complement interestingRow

solvePart2Pure :: [String] -> Int -> Int -> Int
solvePart2Pure input minCoord maxCoord = let
  beacons = map parseLine input
  (x, y) = findBeacon beacons minCoord maxCoord
  in (4000000 * x) + y

solvePart2 = do
  text <- readFile "data/input15.txt"
  return $ solvePart2Pure (lines text) 0 4000000

--  if max1 < min2 then they are disjoint and we are done
--  if max1 >= min2

type Range = (Int, Int)
type RangeSet = Set Range
mergeRanges :: Range -> Range -> Maybe Range
mergeRanges (min1, max1) (min2, max2)
  | min1 > min2 = mergeRanges (min2, max2) (min1, max1)
  | max1 < min2 = Nothing
  | otherwise = Just (min1, max max1 max2)

-- this only works if the ranges go in order such that
-- the first range is the first candidate for merging
-- and as soon as you find one that is not a candidate, you are done
insertRangeInList :: [Range] -> Range -> [Range]
insertRangeInList [] new = [new]
insertRangeInList (x:xs) new = case mergeRanges new x of
  Just merged -> insertRangeInList xs merged
  Nothing -> new:(x:xs)

insertRange :: RangeSet -> Range -> RangeSet
insertRange set (newMin, newMax) = let
  firstFunc (_, b) = b < newMin
  (tooLow, possible) = Set.spanAntitone firstFunc set
  fromPossible = Set.fromList $ insertRangeInList (Set.toList possible) (newMin, newMax)
  in Set.union tooLow fromPossible

mergeSets :: RangeSet -> RangeSet -> RangeSet
mergeSets s1 s2 = foldl insertRange s1 (Set.toList s2)

mergeAllSets :: [RangeSet] -> RangeSet
mergeAllSets allSets = foldl mergeSets Set.empty allSets

rangeWithoutBeaconInRow :: ClosestBeacon -> Int -> Int -> Int -> Maybe Range
rangeWithoutBeaconInRow ((sx, sy), (bx, by)) row minCoord maxCoord = let
  mDistance = abs (sx - bx) + abs (sy - by)
  rowDistanceFromSensor = abs (row - sy)
  remainingDistance = mDistance - rowDistanceFromSensor
  proposedMin = max minCoord (sx - remainingDistance)
  proposedMax = min maxCoord (sx + remainingDistance)
  rangeIsAllowed = remainingDistance >= 0 && proposedMax >= proposedMin
  allowedRange :: Maybe Range
  allowedRange = if rangeIsAllowed
                     then Just (proposedMin, proposedMax)
                     else Nothing
  in allowedRange

allEmptyRangesInRow :: [ClosestBeacon] -> Int -> Int -> Int -> RangeSet
allEmptyRangesInRow beacons row minCoord maxCoord = let
  findRange beacon = rangeWithoutBeaconInRow beacon row minCoord maxCoord
  listOfRanges = catMaybes $ map findRange beacons
  rangeSet = foldl insertRange Set.empty listOfRanges
  in rangeSet

findBeacon2 :: [ClosestBeacon] -> Int -> Int -> Coords
findBeacon2 beacons minCoord maxCoord = let
  checkRow :: Int -> (Int, [Range])
  checkRow row = (row, Set.toList $ allEmptyRangesInRow beacons row minCoord maxCoord)
  allResults = map checkRow [minCoord..maxCoord]
  boring (_, ls) = ls == [(minCoord, maxCoord)]
  interestingResult :: (Int, [Range])
  interestingResult = head $ filter (not . boring) allResults
  (y, stupidCoords) = interestingResult
  x = 1 + (snd $ head stupidCoords)
  in (x, y)

beaconsIO = fmap (map parseLine . lines) $ readFile "data/input15.txt"