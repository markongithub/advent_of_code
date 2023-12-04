module Days.Day04 (runDay) where

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

cardParser :: Parser Card
cardParser = do
  string "Card"
  many1 (char ' ')
  cardNumStr <- many1 digit
  string ": "
  winningNumbers <- numbersParser
  char '|'
  myNumbers <- numbersParser
  return (read cardNumStr, winningNumbers, myNumbers)

inputParser :: Parser Input
inputParser = cardParser `sepBy` endOfLine

------------ TYPES ------------
type Card = (Int, [Int], [Int])
type Input = [Card]

type OutputA = Int

type OutputB = Void

------------ PART A ------------

matches :: Card -> [Int]
matches (_, winners, mine) = let
  winnerSet = Set.fromList winners
  in filter (\n -> Set.member n winnerSet) mine

cardValue :: Card -> Int
cardValue card = case (length $ matches card) of
  0 -> 0
  l -> 2 ^ (l - 1)

partA :: Input -> OutputA
partA cards = sum $ map cardValue cards

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
