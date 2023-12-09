module Days.Day09 (runDay) where

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
intParserThatActuallyWorks :: Parser Int
intParserThatActuallyWorks = do
  digits <- many1 (satisfy $ inClass "01234566789-")
  return $ read digits

inputParser :: Parser Input
inputParser = do
  listOfLists <- (intParserThatActuallyWorks `sepBy` (char ' ')) `sepBy` endOfLine
  -- yeah that's right I'm converting it to string and back because fuck this
  return $ filter (not . null) listOfLists

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

differences :: [Int] -> [Int]
differences [] = error "this should not happen"
differences [x] = []
differences (x:(y:ys)) = (y - x):(differences (y:ys))

recurseDifferences :: [Int] -> [[Int]]
recurseDifferences input = case (all (== 0) input) of
   True -> [input]
   False -> input:(recurseDifferences (differences input))

appendToAll0 :: [[Int]] -> Int -> [[Int]]
appendToAll0 [] _ = []
appendToAll0 ([]:ys) _ = error "inner list is empty"
appendToAll0 ((x:xs):ys) v = let
  topList = (v + x):(x:xs)
  in topList:(appendToAll0 ys (v + x))

appendToAll :: [[Int]] -> [[Int]]
appendToAll xs = appendToAll0 (reverse (map reverse xs)) 0

interpolatedValue :: Bool -> [Int] -> Int
interpolatedValue isPart2 list = let
  recursed = if isPart2 then recurseDifferences (reverse list) else recurseDifferences list
  appended = appendToAll recursed
  in head $ last appended

partA :: Input -> OutputA
partA input = sum $ map (interpolatedValue False) input

------------ PART B ------------
partB :: Input -> OutputB
partB input = sum $ map (interpolatedValue True) input
