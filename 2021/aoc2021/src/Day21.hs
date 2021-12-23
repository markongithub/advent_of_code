module Day21 where

import Data.Map (Map)
import qualified Data.Map as Map

data GameState = GameState Int Int Int Int Int Bool
  deriving (Eq, Show)

applyRollSet :: GameState -> Int -> GameState
applyRollSet (GameState pos1 pos2 score1 score2 totalRolls isTurn1) rollSum = let
  addRolls x = (x + rollSum) `mod` 10
  (newPos1, newPos2) = if isTurn1 then (addRolls pos1, pos2) else (pos1, addRolls pos2)
  addScore oldScore space = if space == 0 then oldScore + 10 else oldScore + space
  (newScore1, newScore2) = if isTurn1 then (addScore score1 newPos1, score2) else (score1, addScore score2 newPos2)
  in GameState newPos1 newPos2 newScore1 newScore2 (totalRolls + 3) (not isTurn1)

hasWinner :: Int -> GameState -> Bool
hasWinner threshold (GameState _ _ score1 score2 _ _) = (score1 >= threshold) || (score2 >= threshold)

applyRollsUntilEnd :: GameState -> [Int] -> GameState
applyRollsUntilEnd gs [] = gs
applyRollsUntilEnd gs rs = let
  (nextRolls, remainder) = splitAt 3 rs
  nextState = applyRollSet gs (sum nextRolls)
  in case (hasWinner 1000 gs) of
    True -> gs
    False -> applyRollsUntilEnd nextState remainder

part1Answer :: Int -> Int -> Int
part1Answer start1 start2 = let
  infiniteRolls = concat (repeat [1..100])
  initialState = GameState start1 start2 0 0 0 True
  finalState = applyRollsUntilEnd initialState infiniteRolls
  GameState _ _ score1 score2 totalRolls _ = finalState
  loserScore = if score1 > score2 then score2 else score1
  in loserScore * totalRolls

part1Test = part1Answer 4 8
solvePart1 = part1Answer 8 10 -- my puzzle input
