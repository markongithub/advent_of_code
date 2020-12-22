module Day22 where

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

size :: Deck -> Int
-- this is stupidly inefficient, maybe I should flip it here
size (Queue front back) = length front + length back

type GameHistory = Set ([Int], [Int])

firstN :: Deck -> Int -> Deck
firstN q n = Queue (take n $ toList q) []

-- don't call this if either deck is empty
advanceGame2 :: Int -> (Deck, Deck) -> (Deck, Deck)
advanceGame2 level (d1, d2) = let
  (c1, r1) = dequeue d1
  (c2, r2) = dequeue d2
  canRecurse = size r1 >= c1 && size r2 >= c2
  playRecursive = playGame2 Set.empty (level + 1) (firstN r1 c1, firstN r2 c2)
  p1WinsRecursion = fst $ playRecursive
  p1Wins = if canRecurse then p1WinsRecursion else (c1 > c2)
  (winnerCard, loserCard) = if p1Wins then (c1, c2) else (c2, c1)
  expandWinnerDeck d = enqueue loserCard $ enqueue winnerCard d
  newP1Deck = if p1Wins then (expandWinnerDeck r1) else r1
  newP2Deck = if p1Wins then r2 else (expandWinnerDeck r2)
  in (newP1Deck, newP2Deck)

playGame2 :: GameHistory -> Int -> (Deck, Deck) -> (Bool, Deck)
playGame2 history level (d1, d2)
  | Set.member setKey history = (True, d1)
  | empty d1 = (False, d2)
  | empty d2 = (True, d1)
  | otherwise = playGame2 newHistory level $ advanceGame2 level (d1, d2)
  where setKey = (toList d1, toList d2)
        newHistory = Set.insert setKey history

solvePart2Func :: (Deck, Deck) -> Int
solvePart2Func (d1, d2) = let
  winningDeck = snd $ playGame2 Set.empty 0 (d1, d2)
  in tallyScore winningDeck

solvePart2 :: IO Int
solvePart2 = let
  text = readFile "data/input22.txt"
  in fmap (solvePart2Func . parseInput . lines) text
