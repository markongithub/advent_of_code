module Days.Day06 (runDay) where

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

inputParser :: Parser Input
inputParser = do
  string "Time: "
  timeNums <- numbersParser
  many' (char ' ')
  endOfLine
  string "Distance: "
  distanceNums <- numbersParser
  return (timeNums, distanceNums)

------------ TYPES ------------
type Race = (Int, Int)
type Input = ([Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART A ------------

inputToRaces :: Input -> [Race]
inputToRaces (times, distances) = zip times distances

countRecordBreakingTimes :: Race -> Int
countRecordBreakingTimes (time, distance) = let
  t = fromIntegral time
  d = fromIntegral distance
  minTime = ceiling $ (sqrt ((t * t) - (4 * d)) - t) / (-2.0)
  maxTime =  floor $ (sqrt ((t * t) - (4 * d)) + t) / 2.0
  in 1 + maxTime - minTime

partA :: Input -> OutputA
partA input = let
  races = inputToRaces input
  countsByRace = map countRecordBreakingTimes races
  in product countsByRace

------------ PART B ------------
concatInts :: [Int] -> Int
concatInts ints = read $ concat $ map show ints

partB :: Input -> OutputB
partB (times, distances) = let
  oneBigTime = concatInts times
  oneBigDistance = concatInts distances
  in countRecordBreakingTimes (oneBigTime, oneBigDistance)
