module Day23 where

import Data.Foldable (toList)
import Data.Maybe (isNothing)
import Data.Sequence (Seq, Seq((:<|)), Seq((:|>)), (><))
import qualified Data.Sequence as Seq
import Debug.Trace (trace)

data GameState = GameState (Seq Int) Int Int deriving (Eq, Show)

fromList :: [Int] -> GameState
fromList ls = GameState (Seq.fromList ls) (minimum ls) (maximum ls)

findDestination :: Int -> Int -> Int -> Seq Int -> Int
findDestination current minLabel maxLabel forbidden
  | current < minLabel = findDestination maxLabel minLabel maxLabel forbidden
  | isNothing (Seq.elemIndexL current forbidden) = current
  | otherwise = findDestination (pred current) minLabel maxLabel forbidden

playTurn :: GameState -> GameState
playTurn g = let
  GameState (current :<| rest) minLabel maxLabel = g
  (pickedUp, notPickedUp) = Seq.splitAt 3 rest
  destination = findDestination (pred current) minLabel maxLabel pickedUp
  (cupsBeforeDest, cupsFromDest) = Seq.spanl (/= destination) notPickedUp
  cupsAfterDest = Seq.drop 1 cupsFromDest
  finalCups = (cupsBeforeDest >< (destination :<| pickedUp)) >< (cupsAfterDest :|> current)
  in GameState finalCups minLabel maxLabel

playNTurns :: GameState -> Int -> GameState
playNTurns state n
  | n == 0 = state
  | (n `mod` 100 == 0) && trace ("First cup is " ++ (show $ current) ++ " with " ++ show n ++ " turns left.") False = undefined
  | otherwise = seq nextState $ playNTurns nextState (n-1)
  where nextState = playTurn $! state
        GameState (current :<| rest) minLabel maxLabel = state

labelsAfter1 :: GameState -> String
labelsAfter1 (GameState cups _ _) = let
  (cupsBefore1, cupsFrom1) = Seq.spanl (/= 1) cups
  cupsAfter1 = Seq.drop 1 cupsFrom1
  finalSeq = cupsAfter1 >< cupsBefore1
  in concat (map show $ toList finalSeq)

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
  (cupsBefore1, cupsFrom1) = Seq.spanl (/= 1) cups
  cupsAfter1 = Seq.drop 1 cupsFrom1
  numCupsAfter1 = Seq.length cupsAfter1
  twoCups = if (numCupsAfter1 >= 2) then (Seq.take 2 cupsAfter1) 
              else (cupsAfter1 >< (Seq.take (2 - numCupsAfter1) cupsBefore1))
  finalList = toList twoCups
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

takeLL :: Int -> SmartLL -> ([Int], SmartLL)
takeLL 0 xs = ([], xs)
takeLL n [] = error "ran out of list"
takeLL n ([]:xs) = takeLL n xs
takeLL n ((x:xs):ys) = let
  (a, b) = takeLL (pred n) (xs:ys)
  in (x:a, b)

splitLL :: (Int -> Bool) -> SmartLL -> ([Int], SmartLL)
splitLL p [] = error "can't split an empty list"
splitLL p ([]:xs) = splitLL p xs
splitLL p ((x:xs):ys) = let
  allTheRest = (xs:ys)
  done = ([], allTheRest)
  (continueA, continueB) = splitLL p allTheRest
  continue = (x:continueA, continueB)
  in if (p x) then done else continue


