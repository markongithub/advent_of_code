module Day23 where

import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

data GameState = GameState ![Int] !Int !Int deriving (Eq, Show)

fromList :: [Int] -> GameState
fromList ls = GameState ls (minimum ls) (maximum ls)

destinationCandidates :: GameState -> [Int]
destinationCandidates (GameState (current:rest) minLabel maxLabel) = let
  downToMin = takeWhile (>= minLabel) $ iterate pred (current - 1)
  downFromMax = takeWhile (> current) $ iterate pred maxLabel
  in downToMin ++ downFromMax

playTurn :: GameState -> GameState
playTurn g = let
  GameState (current:rest) minLabel maxLabel = g
  pickedUp = take 3 rest
  orderedCandidates = destinationCandidates g
  destination = head  $ dropWhile (\c -> any (==c) pickedUp) orderedCandidates
  listBefore = takeWhile (/= destination) $ drop 3 rest
  listAfter = drop (length listBefore + 4) rest
  finalList = listBefore ++ (destination:pickedUp) ++ listAfter ++ [current]
  in GameState finalList minLabel maxLabel

playNTurns :: GameState -> Int -> GameState
playNTurns state n
  | n == 0 = state
  | (n `mod` 10 == 0) && trace ("Last cup is " ++ (show $ last rest) ++ " with " ++ show n ++ " turns left.") False = undefined
  | otherwise = seq nextState $ playNTurns nextState (n-1)
  where nextState = playTurn $! state
        GameState (current:rest) minLabel maxLabel = state

labelsAfter1 :: GameState -> String
labelsAfter1 (GameState cups _ _) = let
  cupsBefore1 = takeWhile (/= 1) cups
  cupsAfter1 = drop (length cupsBefore1 + 1) cups
  finalList = cupsAfter1 ++ cupsBefore1
  in concat (map show finalList)

solvePart1Func :: GameState -> String
solvePart1Func g = labelsAfter1 $ playNTurns g 100

part1Input = [6,2,4,3,9,7,1,5,8]

solvePart1 :: String
solvePart1 = solvePart1Func (fromList part1Input)

initialLongList :: [Int] -> [Int]
initialLongList cups = let
  previousMax = maximum cups -- should always be 9 really
  additionalCups = [(previousMax + 1)..1000000]
  in cups ++ additionalCups

twoCupsAfter1 :: GameState -> (Int, Int)
twoCupsAfter1 (GameState cups _ _) = let
  cupsAfter1 = tail $ dropWhile (/= 1) cups
  twoCups = if (length cupsAfter1 >= 2) then (take 2 cupsAfter1) 
              else (cupsAfter1 ++ (take (2 - (length cupsAfter1)) cups))
  in (twoCups!!0, twoCups!!1)

part2Product :: GameState -> Int
part2Product g = let
  (x, y) = twoCupsAfter1 g
  in x * y

solvePart2 :: Int
solvePart2 = let
  initialState = fromList (initialLongList part1Input)
  finalState = playNTurns initialState 10000000
  in part2Product finalState
