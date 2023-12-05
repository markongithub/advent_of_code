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

findDestination :: MappingSet -> Int -> Int
findDestination mSet source = let
  lookupKey = (source, maxBound, maxBound)
  (start, destStart, size) = case (Set.lookupLT lookupKey mSet) of
    Nothing -> (0, 0, 0)
    Just result -> result
  in if source > (start + size - 1) then source else (destStart + source - start)

seedToLocation :: [MappingSet] -> Int -> Int
seedToLocation [] location = location
seedToLocation (x:xs) curValue = let
  nextStep = findDestination x curValue
  in seedToLocation xs nextStep

partA :: Input -> OutputA
partA (seeds, rangeMaps)= let
  mSets = map makeMappingSet rangeMaps
  in minimum $ map (seedToLocation mSets) seeds

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

expandSeeds :: [(Int, Int)] -> [Int]
expandSeeds []  = []
expandSeeds ((min1, max1):xs) = [min1..max1] ++ expandSeeds xs

-- expandSeeds :: [Int] -> [Int]
-- expandSeeds xs = Set.toList $ expandSeeds0 xs Set.empty

-- expandSeeds0 :: [Int] -> Set Int -> Set Int
-- expandSeeds0 [] accu = accu
-- expandSeeds0 [x] _ = error "why was there an odd number of numbers"
-- expandSeeds0 (x:(y:xs)) accu = let
--   newSeeds = traceShow ("accu size is " ++ show (Set.size accu) ++ " and I am about to insert " ++ show y) $ Data.List.take y [x..]
--   newAccu = foldl (flip Set.insert) accu newSeeds
--   in expandSeeds0 xs newAccu

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
  debugMessage = show seedRanges ++ " are the seed ranges and " ++ (show $ map Set.size mSets) ++ " are the mapping set sizes"
  in traceShow debugMessage $ minimum $ map (seedToLocation mSets) (expandSeeds seedRanges)
