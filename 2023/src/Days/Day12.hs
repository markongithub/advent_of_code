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

prependToListInReverse :: [a] -> [a] -> [a]
prependToListInReverse dest [] = dest
prependToListInReverse dest (x:xs) = prependToListInReverse (x:dest) xs
horribleRecursion :: String -> [Int] -> [String]
horribleRecursion cs ns = map reverse $ horribleRecursion0 cs ns []

horribleRecursion0 :: String -> [Int] -> String -> [String]
horribleRecursion0 [] [] accu = [accu]
horribleRecursion0 (x:xs) [] accu = case x of
  '#' -> []
  _ -> horribleRecursion0 xs [] ('.':accu)
horribleRecursion0 [] (x:xs) accu = []
horribleRecursion0 cs0 (n:ns) accu = let
  cs = traceShow ("horribleRecursion0 " ++ cs0 ++ show (n:ns) ++  accu) cs0
  remainderAfterMatch = drop n cs
  accuWithMatchBase = prependToListInReverse accu (replaceAll (Data.List.take n cs) '?' '#')
  accuWithMatch = case remainderAfterMatch of
    [] -> accuWithMatchBase
    _  -> '.':accuWithMatchBase
  recurseWithMatch = horribleRecursion0 (drop 1 remainderAfterMatch) ns accuWithMatch
  accuWithoutMatch = ('.':accu)
  recurseWithoutMatch = horribleRecursion0 (tail cs) (n:ns) accuWithoutMatch
  mustMatch = (head cs) == '#'
  in case (canMatchN cs n, mustMatch) of
    (True, True) -> recurseWithMatch
    (True, False) -> recurseWithMatch ++ recurseWithoutMatch
    (False, True) -> []
    (False, False) -> recurseWithoutMatch

arrangements :: (String, [Int]) -> Int
arrangements (str, nums) = length $ horribleRecursion str nums

partA :: Input -> (OutputA, [Int])
partA input = let
  allValues = map arrangements input
  in (sum allValues, allValues)

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
