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

type OutputB = Int

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

type Inventory = Map Int Int

updateInventory :: Inventory -> [Int] -> Int -> Inventory
updateInventory inventory newCards countOfEach = let
  addSingleCard m card = Map.insertWith (+) card countOfEach m
  in foldl addSingleCard inventory newCards

initialInventory :: Input -> Inventory
initialInventory cards = let
  numbers = map (\(n, _, _) -> n) cards
  keyVals = zip numbers (repeat 1)
  in Map.fromList keyVals

processCard :: Inventory -> Card -> Inventory
processCard inventory card = let
  (index, _, _) = card
  countHeld :: Int
  countHeld = Map.findWithDefault 0 index inventory
  numWinners = length $ matches card
  cardsWon = Data.List.take numWinners [(index + 1)..]
  newInventory = updateInventory inventory cardsWon countHeld
  in newInventory

partB :: Input -> OutputB
partB cards = let
  finalCards = foldl processCard (initialInventory cards) cards
  in sum $ Map.elems finalCards
