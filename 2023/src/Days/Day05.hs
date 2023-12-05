module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void

import Debug.Trace (traceShow)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
numberParser :: Parser Int
numberParser = do
  many' (char ' ')
  numStr <- many1 digit
  many' (char ' ')
  return (read numStr)

numbersParser :: Parser [Int]
numbersParser = many1 numberParser

listToTriple :: [Int] -> (Int, Int, Int)
listToTriple [x, y, z] = (x, y, z)
listToTriple _ = error "that list was not of length 3"

rangeMapParser :: Parser RangeMap
rangeMapParser = do
  many' endOfLine
  mapName <- manyTill anyChar (string " map:")
  endOfLine
  ranges <- numbersParser `sepBy` endOfLine
  return (mapName, map listToTriple ranges)

inputParser :: Parser Input
inputParser = do
  string "seeds: "
  seedNums <- numbersParser
  endOfLine
  many' (char ' ')
  endOfLine
  rangeMaps <- many1 rangeMapParser
  return (seedNums, rangeMaps)



------------ TYPES ------------
type Range = (Int, Int, Int)
type RangeMap = (String, [Range])
type Input = ([Int], [RangeMap])

type OutputA = Int

type OutputB = Int

------------ PART A ------------

type MappingSet = Set (Int, Int, Int)
makeMappingSet :: RangeMap -> MappingSet
makeMappingSet (_, ranges) = let
  putSourceFirst (dest, source, size) = (source, dest, size)
  in Set.fromList $ map putSourceFirst ranges

findDestination :: MappingSet -> Int -> Int -> (Int, Int)
findDestination mSet source skippable = let
  lookupKey = (source, maxBound, maxBound)
  fst3 (x, _, _) = x
  handleBelowMin = (source, ((fst3 (Set.findMin mSet)) - source))
  handleResult start destStart size = if source > (start + size - 1) then handleAboveMax else ((destStart + source - start), (start + size - source))
  handleAboveMax = case (Set.lookupGT lookupKey mSet) of
    Nothing -> (source, skippable)
    Just (start, destStart, size) -> (source, min skippable (start - source))
  in case (Set.lookupLT lookupKey mSet) of
    Nothing -> handleBelowMin
    Just (start, destStart, size) -> handleResult start destStart size

seedToLocation :: [MappingSet] -> Int -> Int -> (Int, Int)
seedToLocation [] location skippable = (location, skippable)
seedToLocation (x:xs) curValue skippable = let
  (nextStep, otherSkippable) = findDestination x curValue skippable
  in seedToLocation xs nextStep (min skippable otherSkippable)

partA :: Input -> OutputA
partA (seeds, rangeMaps)= let
  mSets = map makeMappingSet rangeMaps
  in minimum $ map (fst . (\s -> seedToLocation mSets s 0)) seeds

------------ PART B ------------

dedup0 :: [(Int, Int)] -> [(Int, Int)]
dedup0 [] = []
dedup0 [x] = [x]
dedup0 ((min1, max1):((min2, max2):xs))
  | min1 > min2 = error "min1 should never be greater than min2"
  | max1 < min2 = (min1, max1):(dedup0 ((min2, max2): xs))
  | otherwise = dedup0 ((min1, (max max1 max2)):xs)

dedup :: [(Int, Int)] -> [(Int, Int)]
dedup xs = dedup0 $ sort xs

seedsToRanges :: [Int] -> [(Int, Int)]
seedsToRanges xs = dedup $ seedsToRanges0 xs

seedsToRanges0 :: [Int] -> [(Int, Int)]
seedsToRanges0 []  = []
seedsToRanges0 [x] = error "why was there an odd number of numbers"
seedsToRanges0 (x:(y:xs)) = (x, x + y - 1):(seedsToRanges0 xs)

minimumByRange :: [MappingSet] -> (Int, Int) -> Int
minimumByRange mSets (min1, max1) = let
  debugMessage = "minimumByRange " ++ show (min1, max1)
  (thisLocation, skippable) = seedToLocation mSets min1 maxBound
  in case (min1 + skippable >= max1) of
    True -> thisLocation
    False -> min thisLocation (minimumByRange mSets (min1 + (max skippable 1), max1))

countSeeds :: [Int] -> Int
countSeeds [] = 0
countSeeds [x] = error "why was there an odd number of numbers"
countSeeds (x:(y:xs)) = y + countSeeds xs

-- calculateWithCache :: Ord k => (k -> a) -> Map k a -> k -> (a, Map k a)
-- calculateWithCache func oldCache input = case (Map.lookup input oldCache) of
--   Nothing -> 
partB :: Input -> OutputB
partB (seeds, rangeMaps)= let
  seedRanges = traceShow ("count of seeds before deduping: " ++ show (countSeeds seeds)) $ seedsToRanges seeds
  mSets = map makeMappingSet rangeMaps
  debugMessage = show seedRanges ++ " are the seed ranges with " ++ (show $ map Set.size mSets) ++ " as the mapping set sizes"
  in traceShow debugMessage $ minimum $ map (minimumByRange mSets) seedRanges
