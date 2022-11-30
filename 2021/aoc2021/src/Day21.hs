module Day21 where

import Data.Map (Map, (!))
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

possibleRolls :: [Int]
possibleRolls = map sum $ [[x,y,z] | x <- [1..3], y<-[1..3], z<-[1..3]]

rollFrequencies :: [(Int, Int)]
rollFrequencies = let
  updateRollMap :: Map Int Int -> Int -> Map Int Int
  updateRollMap m k = Map.insertWith (+) k 1 m
  rollMap = foldl updateRollMap Map.empty possibleRolls
  in Map.toList rollMap

type CacheKey = (Int, Int, Int, Int, Bool)
type GameCache = Map CacheKey (Int, Int)

cacheKey :: GameState -> CacheKey
cacheKey (GameState a b c d _ e) = (a,b,c,d,e)

countWinners :: GameCache -> GameState -> (GameCache, Int, Int)
countWinners cache gs
  | hasWinner 21 gs && score1 > score2 = (cache, 1,0)
  | hasWinner 21 gs = (cache, 0,1)
  | Map.member key cache = outcomeFromCache
  | otherwise = recursiveOutcome
  where
    GameState _ _ score1 score2 _ _ = gs
    key = cacheKey gs
    (cached1, cached2) = cache!key
    outcomeFromCache = (cache, cached1, cached2)
    suboutcomes = map (applyRollSet gs) (map fst rollFrequencies)
    frequencies = map snd rollFrequencies
    (newCache, wins1, wins2) = recurseAcross cache 0 0 suboutcomes frequencies
    finalCache = Map.insert key (wins1, wins2) newCache
    recursiveOutcome = (finalCache, wins1, wins2)

recurseAcross :: GameCache -> Int -> Int -> [GameState] -> [Int] -> (GameCache, Int, Int)
recurseAcross cache accu1 accu2 [] _ = (cache, accu1, accu2)
recurseAcross cache accu1 accu2 (g:gs) (m:ms) = let
  (newCache, sub1, sub2) = countWinners cache g
  newAccu1 = accu1 + (sub1 * m)
  newAccu2 = accu2 + (sub2 * m)
  in recurseAcross newCache newAccu1 newAccu2 gs ms


part2Answer :: Int -> Int -> Int
part2Answer start1 start2 = let
  initialState = GameState start1 start2 0 0 0 True
  (_,x,y) = countWinners Map.empty initialState
  in max x y

part2Test = part2Answer 4 8
solvePart2 = part2Answer 8 10 -- my puzzle input
