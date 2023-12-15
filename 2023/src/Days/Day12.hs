module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace (traceShow)
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
numbersParser :: Parser [Int]
numbersParser = do
  numberStrs <- many1 digit `sepBy` (char ',')
  return (map read numberStrs)

lineParser :: Parser (String, [Int])
lineParser = do
  string <- many1 (satisfy $ inClass "?.#")
  char ' '
  numbers <- numbersParser
  return (string, numbers)

inputParser :: Parser Input
inputParser = lineParser `sepBy` endOfLine

------------ TYPES ------------
type Input = [(String, [Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

canBeWorking :: Char -> Bool
canBeWorking c = c == '.' || c == '?'

canBeBroken :: Char -> Bool
canBeBroken c = c == '#' || c == '?'

replaceAll :: Eq a => [a] -> a -> a -> [a]
replaceAll [] _ _ = []
replaceAll (x:xs) from to = if x == from then (to:(replaceAll xs from to)) else (x:(replaceAll xs from to))

canMatchN :: String -> Int -> Bool
canMatchN [] 0 = True
canMatchN [] n = False
canMatchN (x:xs) 0 = canBeWorking x
canMatchN (x:xs) n = canBeBroken x && canMatchN xs (n-1)

type Day12Cache = Map (String, [Int]) Int

horribleRecursion :: String -> [Int] -> Int
horribleRecursion cs ns = fst $ horribleRecursion0 cs ns 0 Map.empty

horribleRecursion0 :: String -> [Int] -> Int -> Day12Cache -> (Int, Day12Cache)
horribleRecursion0 [] [] _ cache = (1, cache)
horribleRecursion0 (x:xs) [] depth cache = case x of
  '#' -> (0, cache)
  _ -> horribleRecursion0 xs [] (depth + 1) cache
horribleRecursion0 [] (x:xs) _ cache = (0, cache)
horribleRecursion0 cs0 (n:ns) depth cache = let
  cs = cs0 -- traceShow ("horribleRecursion0 " ++ cs0 ++ show (n:ns) ++  accu) cs0
  remainderAfterMatch = drop n cs
  shouldRecurseWithMatch = canMatchN cs n
  (recurseWithMatchNum, cache2) = if shouldRecurseWithMatch then horribleRecursion0 (drop 1 remainderAfterMatch) ns (depth + n + 1) cache else (0, cache)
  recurseWithMatch = fst $ horribleRecursion0 (drop 1 remainderAfterMatch) ns (depth + n + 1) cache
  recurseWithoutMatch = fst $ horribleRecursion0 (tail cs) (n:ns) (depth + 1) cache
  mustMatch = (head cs) == '#'
  outputNumber = case (canMatchN cs n, mustMatch) of
    (True, True) -> recurseWithMatchNum
    (True, False) -> recurseWithMatchNum + recurseWithoutMatch
    (False, True) -> 0
    (False, False) -> recurseWithoutMatch
  in (outputNumber, cache)

arrangements :: (String, [Int]) -> Int
arrangements (str, nums) = horribleRecursion str nums

partA :: Input -> (OutputA, [Int])
partA input = let
  allValues = map arrangements input
  in (sum allValues, allValues)

------------ PART B ------------
cons :: [a] -> a -> [a]
cons xs x = (x:xs)

expandList :: String -> String
expandList s = let
  laterList = cons s '?'
  laterLists = Data.List.take 4 $ repeat laterList
  in s ++ (concat laterLists)

expandInput (str, nums) = (expandList str, concat $ Data.List.take 5 $ repeat nums)

cacheLookup :: (Ord k) => Map k a -> (k -> a) -> k -> (a, Map k a)
cacheLookup cache func input = let
  slowResult = func input
  newCache = Map.insert input slowResult cache
  in case Map.lookup input cache of
    Just result -> (result, cache)
    Nothing -> (slowResult, newCache)

-- partB :: Input -> [Int] -- , OutputB)
partB input = let
  allValues = map (arrangements . expandInput) input
  in (3, head input, allValues, sum allValues) -- (allValues, sum allValues)
