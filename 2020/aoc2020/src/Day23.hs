module Day23 where

import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data GameState = GameState [Int] deriving (Eq, Show)

playTurn :: GameState -> GameState
playTurn (GameState (current:rest)) = let
  pickedUp = take 3 rest
  allDestCandidates = reverse $ sort (drop 3 rest)
  orderedCandidates = (dropWhile (>= current) allDestCandidates) ++ allDestCandidates
  destination = head orderedCandidates
  listBefore = takeWhile (/= destination) $ drop 3 rest
  listAfter = drop (length listBefore + 4) rest
  finalList = listBefore ++ (destination:pickedUp) ++ listAfter ++ [current]
  in GameState finalList

playNTurns :: GameState -> Int -> GameState
playNTurns state n = (iterate playTurn state)!!n

labelsAfter1 :: GameState -> String
labelsAfter1 (GameState cups) = let
  cupsBefore1 = takeWhile (/= 1) cups
  cupsAfter1 = drop (length cupsBefore1 + 1) cups
  finalList = cupsAfter1 ++ cupsBefore1
  in concat (map show finalList)

solvePart1Func :: GameState -> String
solvePart1Func g = labelsAfter1 $ playNTurns g 100

part1Input = [6,2,4,3,9,7,1,5,8]

solvePart1 :: String
solvePart1 = solvePart1Func (GameState part1Input)
