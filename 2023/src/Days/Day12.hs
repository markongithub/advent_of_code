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
horribleRecursion cs ns = fst $ horribleRecursion0 cs ns (length cs) Map.empty

horribleRecursion0 :: String -> [Int] -> Int -> Day12Cache -> (Int, Day12Cache)
horribleRecursion0 [] [] _ cache = (1, cache)
horribleRecursion0 (x:xs) [] toGo cache = case x of
  '#' -> (0, cache)
  _ -> horribleRecursion0 xs [] (toGo - 1) cache
horribleRecursion0 [] (x:xs) _ cache = (0, cache)
horribleRecursion0 cs0 (n:ns) toGo cache = let
  cs = cs0 -- if toGo < 100 then cs0 else traceShow ("horribleRecursion0 " ++ cs0 ++ show (n:ns, toGo)) cs0
  cacheKey = (cs, (n:ns))
  remainderAfterMatch = drop n cs
  shouldRecurseWithMatch = canMatchN cs n
  (recurseWithMatchNum, cache2) = if shouldRecurseWithMatch then horribleRecursion0 (drop 1 remainderAfterMatch) ns (toGo - (n + 1)) cache else (0, cache)
  (recurseWithoutMatchNum, cache3) = if mustMatch then (0, cache2) else horribleRecursion0 (tail cs) (n:ns) (toGo - 1) cache2
  mustMatch = (head cs) == '#'
  outputNumber = recurseWithMatchNum + recurseWithoutMatchNum
  maybeFromCache = if toGo > 20 then Map.lookup cacheKey cache else Nothing
  finalCache = if toGo > 20 then Map.insert cacheKey outputNumber cache3 else cache3
  in case maybeFromCache of
    Nothing -> (outputNumber, finalCache)
    Just result -> (result, cache) -- traceShow ("cache hit at toGo " ++ show toGo) (result, cache)

arrangements :: (String, [Int]) -> Int
arrangements (str, nums) = horribleRecursion str nums

partA :: Input -> OutputA
partA input = let
  allValues = map arrangements input
  in sum allValues

------------ PART B ------------
cons :: [a] -> a -> [a]
cons xs x = (x:xs)

expandList :: String -> String
expandList s = let
  laterList = cons s '?'
  laterLists = Data.List.take 4 $ repeat laterList
  in s ++ (concat laterLists)

expandInput (str, nums) = (expandList str, concat $ Data.List.take 5 $ repeat nums)

-- partB :: Input -> OutputB
partB input = let
  allValues = map (arrangements . expandInput) input
  in sum allValues
