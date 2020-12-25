module Day23 where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

data GameState = GameState !Int !(Map Int Int) Int Int deriving (Eq, Show)

fromList :: [Int] -> GameState
fromList ls = let
  loopPair = (last ls, head ls)
  allPairs = loopPair:(zip ls (tail ls))
  in GameState (head ls) (Map.fromList allPairs) (minimum ls) (maximum ls)

findDestination :: Int -> Int -> Int -> [Int] -> Int
findDestination current minLabel maxLabel forbidden
  | current < minLabel = findDestination maxLabel minLabel maxLabel forbidden
  | all (/= current) forbidden = current
  | otherwise = findDestination (pred current) minLabel maxLabel forbidden

takeM :: Int -> Int -> Map Int Int -> [Int]
takeM 0 k m = []
takeM n k m = (m!k):(takeM (n - 1) (m!k) m)

cupsInOrder :: GameState -> [Int]
cupsInOrder (GameState current cups _ _) = let
  rest = takeM (Map.size cups - 1) current cups
  in current:rest

playTurn :: GameState -> GameState
playTurn g = let
  GameState current cups minLabel maxLabel = g
  pickedUp = takeM 3 current cups
  destination = findDestination (pred current) minLabel maxLabel pickedUp
  oldCurrentNext = head pickedUp
  oldDestinationNext = cups!destination
  oldPickedUpNext = cups!(last pickedUp)
  newPairs = [(current, oldPickedUpNext), (destination, oldCurrentNext), (last pickedUp, oldDestinationNext)]
  newPairMap = Map.fromList newPairs
  newMap = Map.union newPairMap cups
  newCurrent = oldPickedUpNext
  in GameState newCurrent newMap minLabel maxLabel

strictMod = 100000
playNTurns :: GameState -> Int -> GameState
playNTurns state n
  | n == 0 = state
  | (n `mod` strictMod == 0) && trace ("current cup is " ++ (show $ current) ++ " with " ++ show n ++ " turns left.") False = undefined
  | otherwise = seq nextState $ playNTurns nextState (n-1)
  where nextFunc = playTurn -- if (n `mod` strictMod == 0) then playTurnAndFlatten else playTurn
        nextState = nextFunc $! state
        GameState current cups minLabel maxLabel = state

labelsAfter1 :: GameState -> String
labelsAfter1 (GameState _ cups _ _) = let
  ints = takeM (Map.size cups - 1) 1 cups
  in concat (map show $ ints)

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
twoCupsAfter1 (GameState _ cups _ _) = let
  finalList = takeM 2 1 cups
  in (finalList!!0, finalList!!1)

part2Product :: GameState -> Int
part2Product g = let
  (x, y) = twoCupsAfter1 g
  in x * y

solvePart2 :: Int
solvePart2 = let
  initialState = fromList (initialLongList part1Input)
  finalState = playNTurns initialState 10000000
  in part2Product finalState
