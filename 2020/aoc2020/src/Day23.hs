module Day23 where

import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.Sequence (Seq, Seq((:<|)), Seq((:|>)), (><))
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

data GameState = GameState SmartLL Int Int deriving (Eq, Show)

fromList :: [Int] -> GameState
fromList ls = GameState [ls] (minimum ls) (maximum ls)

findDestination :: Int -> Int -> Int -> [Int] -> Int
findDestination current minLabel maxLabel forbidden
  | current < minLabel = findDestination maxLabel minLabel maxLabel forbidden
  | all (/= current) forbidden = current
  | otherwise = findDestination (pred current) minLabel maxLabel forbidden

playTurn :: GameState -> GameState
playTurn g = let
  GameState cups minLabel maxLabel = g
  (current, rest) = deconstructLL cups
  (pickedUp, notPickedUp) = takeLL 3 rest
  destination = findDestination (pred current) minLabel maxLabel pickedUp
  (cupsBeforeDest, cupsAfterDest) = splitLL (== destination) notPickedUp
  finalCups = [cupsBeforeDest, (destination:pickedUp)] ++ cupsAfterDest ++ [[current]]
  in GameState finalCups minLabel maxLabel

playNTurns :: GameState -> Int -> GameState
playNTurns state n
  | n == 0 = state
  | (n `mod` 100 == 0) && trace ("First cup is " ++ (show $ headLL cups) ++ " with " ++ show n ++ " turns left.") False = undefined
  | otherwise = seq nextState $ playNTurns nextState (n-1)
  where nextState = playTurn $! state
        GameState cups minLabel maxLabel = state

labelsAfter1 :: GameState -> String
labelsAfter1 (GameState cups _ _) = let
  (cupsBefore1, cupsAfter1) = splitLL (== 1) cups
  finalCups = cupsAfter1 ++ [cupsBefore1]
  in concat (map show $ flattenLL finalCups)

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
  (cupsBefore1, cupsAfter1) = splitLL (== 1) cups
  twoFromAfter = fst $ safeTakeLL 2 cupsAfter1
  numCupsAfter1 = length twoFromAfter
  twoCups = if (numCupsAfter1 == 2) then twoFromAfter
              else (twoFromAfter ++ (take (2 - numCupsAfter1) cupsBefore1))
  finalList = twoCups
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

type SmartLL = [[Int]]

headLL :: SmartLL -> Int
headLL [] = error "headLL on empty LL"
headLL ([]:xs) = headLL xs
headLL ((x:xs):ys) = x

deconstructLL :: SmartLL -> (Int, SmartLL)
deconstructLL [] = error "deconstructLL on empty LL"
deconstructLL ([]:xs) = deconstructLL xs
deconstructLL ((x:xs):ys) = (x, (xs:ys))

takeLL :: Int -> SmartLL -> ([Int], SmartLL)
takeLL 0 xs = ([], xs)
takeLL n [] = error "ran out of list"
takeLL n ([]:xs) = takeLL n xs
takeLL n ((x:xs):ys) = let
  (a, b) = takeLL (pred n) (xs:ys)
  in (x:a, b)

safeTakeLL :: Int -> SmartLL -> ([Int], SmartLL)
safeTakeLL 0 xs = ([], xs)
safeTakeLL n [] = ([], [])
safeTakeLL n ([]:xs) = safeTakeLL n xs
safeTakeLL n ((x:xs):ys) = let
  (a, b) = safeTakeLL (pred n) (xs:ys)
  in (x:a, b)

splitLL0 :: (Int -> Bool) -> SmartLL -> [Int] -> ([Int], SmartLL)
splitLL0 p [] _ = error "can't split an empty list"
splitLL0 p ([]:xs) accu = splitLL0 p xs accu
splitLL0 p ((x:xs):ys) accu = let
  allTheRest = (xs:ys)
  done = (reverse accu, allTheRest)
  continue = splitLL0 p allTheRest (x:accu)
  in if (p x) then done else continue

splitLL :: (Int -> Bool) -> SmartLL -> ([Int], SmartLL)
splitLL p xs = splitLL0 p xs []

flattenLL0 :: SmartLL -> [Int] -> [Int]
flattenLL0 [] accu = reverse accu
flattenLL0 ([]:xs) accu = flattenLL0 xs accu
flattenLL0 ((x:xs):ys) accu = flattenLL0 (xs:ys) (x:accu)

flattenLL :: SmartLL -> [Int]
flattenLL [] = []
flattenLL ([]:xs) = flattenLL xs
flattenLL ((x:xs):ys) = x:(flattenLL (xs:ys))
