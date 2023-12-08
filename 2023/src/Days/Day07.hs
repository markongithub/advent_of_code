module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (isDigit, digitToInt)
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
handParser :: Parser Hand
handParser = do
  cards <- many1 (notChar ' ')
  char ' '
  numbers <- many1 digit
  return (cards, read numbers)

inputParser :: Parser Input
inputParser = handParser `sepBy` endOfLine

------------ TYPES ------------

type Hand = (String, Int)

type UniqueCount = Map Char Int

type Input = [Hand]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
cardValue :: Bool -> Char -> Int
cardValue isPart2 c
  | isDigit c = digitToInt c
  | otherwise = case c of
      'A' -> 14
      'K' -> 13
      'Q' -> 12
      'J' -> if isPart2 then 1 else 11
      'T' -> 10
      _ -> error (show c ++ " is not a valid card")

breakTie :: Bool -> Hand -> Hand -> Ordering
breakTie isPart2 (h1, _) (h2, _) = compare (map (cardValue isPart2) h1) (map (cardValue isPart2) h2)

makeUniqueCount :: String -> UniqueCount
makeUniqueCount cards = let
  insertOne m c = Map.insertWith (+) c 1 m
  in foldl insertOne Map.empty cards

handValue :: Hand -> Int
handValue (cards, _) = let
  uniqueCount = makeUniqueCount cards
  frequencies = reverse $ sort $ Map.elems uniqueCount
  in case frequencies of
    [5] -> 6
    [4, 1] -> 5
    [3, 2] -> 4
    [3, 1, 1] -> 3
    [2, 2, 1] -> 2
    [2, 1, 1, 1] -> 1
    [1, 1, 1, 1, 1] -> 0
    _ -> error ("unexpected distribution: " ++ show frequencies)

compareHands :: Bool -> Hand -> Hand -> Ordering
compareHands isPart2 h1 h2 = let
  valueFunc = if isPart2 then handValue2 else handValue
  v1 = valueFunc h1
  v2 = valueFunc h2
  in if v1 == v2 then breakTie isPart2 h1 h2 else compare v1 v2

solveDay7 :: Bool -> Input -> Int
solveDay7 isPart2 hands = let
  sortedHands = sortBy (compareHands isPart2) hands
  handsWithRanks = zip [1..] sortedHands
  winnings (rank, (_, bid)) = rank * bid
  in sum $ map winnings handsWithRanks

partA :: Input -> OutputA
partA hands = solveDay7 False hands

------------ PART B ------------

handValue2 :: Hand -> Int
handValue2 (cards, _) = let
  candidates = filter (/= 'J') $ Set.toList $ Set.fromList cards
  makeTransformation newC = (map (\oldC -> if oldC == 'J' then newC else oldC) cards, undefined)
  allTransformations = case cards of
    "JJJJJ" -> [("JJJJJ", undefined)]
    _ -> map makeTransformation candidates
  in maximum $ map handValue allTransformations

partB :: Input -> OutputB
partB hands = solveDay7 True hands