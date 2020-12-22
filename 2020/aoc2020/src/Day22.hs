module Day22 where

import Common
import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Queue a = Queue [a] [a]

empty :: Queue a -> Bool
empty (Queue xs ys) = null xs && null ys

moveUpBack :: Queue a -> Queue a
moveUpBack (Queue (x:xs) _) = error "Don't call moveUpBack with cards in the front."
moveUpBack (Queue [] back) = Queue (reverse back) []

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue front back) = case front of
  (x:xs) -> (x, Queue xs back)
  []     -> dequeue $ moveUpBack (Queue front back)

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front back) = Queue front (x:back)

type Deck = Queue Int

-- don't call this if either deck is empty
advanceGame :: (Deck, Deck) -> (Deck, Deck)
advanceGame (d1, d2) = let
  (c1, r1) = dequeue d1
  (c2, r2) = dequeue d2
  c1Wins :: (Deck, Deck)
  c1Wins = ((enqueue c2 $ enqueue c1 $ r1), r2)
  c2Wins = (r1, enqueue c1 $ enqueue c2 $ r2)
  in if (c1 > c2) then c1Wins else c2Wins

playGame :: (Deck, Deck) -> Deck
playGame (d1, d2)
  | empty d1 = d2
  | empty d2 = d1
  | otherwise = playGame $ advanceGame (d1, d2)

toList :: Deck -> [Int]
toList (Queue front back) = front ++ (reverse back)

tallyScore :: Deck -> Int
tallyScore (Queue front back) = let
  asReverseList = back ++ (reverse front)
  in sum $ zipWith (*) [1..] asReverseList

testDeck1 :: Deck
testDeck1 = Queue [9,2,6,3,1] []
testDeck2 :: Deck
testDeck2 = Queue [5,8,4,7,10] []

solvePart1Func :: (Deck, Deck) -> Int
solvePart1Func (d1, d2) = let
  winningDeck = playGame (d1, d2)
  in tallyScore winningDeck

parseDeck :: [String] -> Deck
parseDeck strings = let
  cards = map (\s -> (read s :: Int)) strings
  in Queue cards []

parseInput :: [String] -> (Deck, Deck)
parseInput strings = let
  cards1 = takeWhile (not . null) $ tail strings
  cards2 = drop (length cards1 + 3) strings
  in (parseDeck cards1, parseDeck cards2)

solvePart1 :: IO Int
solvePart1 = let
  text = readFile "data/input22.txt"
  in fmap (solvePart1Func . parseInput . lines) text

--solvePart2Func :: [String] -> String
--solvePart2Func input = let
--  solution = solve $ reduceMap $ mapFromInput input
--  allergens = map snd solution
--  in intercalate "," allergens
--
--solvePart2 :: IO String
--solvePart2 = let
--  text = readFile "data/input22.txt"
--  in fmap (solvePart2Func . lines) text
