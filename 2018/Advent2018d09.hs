module Main where

import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

type Marble = Int
type Player = Int

data GameState = GameState { clockwise :: IntMap Marble
                           , counterCW :: IntMap Marble
                           , currentMarble :: Marble
                           , nextAvailableMarble :: Marble
                           , nextPlayer :: Player
                           , numPlayers :: Int
                           , scores :: Map Player Int
                           } deriving Show

initialState :: Int -> GameState
initialState np = GameState (IntMap.fromList [(0, 0)]) (IntMap.fromList [(0, 0)]) 0 1 1 np (Map.fromDistinctAscList $ zip [1..np] (repeat 0))

insertAfter :: GameState -> Marble -> Marble -> GameState
insertAfter state predecessor newM = let
  oldClockwise = clockwise state
  oldCounterCW = counterCW state
  successor = oldClockwise!predecessor
  newClockwise = IntMap.union (IntMap.fromList [ (predecessor, newM)
                                         , (newM, successor) ]) oldClockwise
  newCounterCW = IntMap.union (IntMap.fromList [ (newM, predecessor)
                                         , (successor, newM) ]) oldCounterCW
  in state { clockwise = newClockwise, counterCW = newCounterCW }

delete :: GameState -> Marble -> GameState
delete state badM = let
  oldClockwise = clockwise state
  oldCounterCW = counterCW state
  successor = oldClockwise!badM
  predecessor = oldCounterCW!badM
  newClockwise = IntMap.insert predecessor successor $ IntMap.delete badM oldClockwise
  newCounterCW = IntMap.insert successor predecessor $ IntMap.delete badM oldCounterCW
  in state { clockwise = newClockwise, counterCW = newCounterCW }

insertOneAfterCurrent :: GameState -> Marble -> GameState
insertOneAfterCurrent state newM = let
  predecessor = ((clockwise state)!(currentMarble state))
  in insertAfter state predecessor newM

findNextPlayer :: GameState -> Player
findNextPlayer state = ((nextPlayer state) + 1) `mod` (numPlayers state)

playNormalMove :: GameState -> GameState
playNormalMove state0 = let
  GameState _ _ curM nextM nextP numP _ = state0
  state1 = insertOneAfterCurrent state0 (nextAvailableMarble state0)
  in state1 { nextPlayer = findNextPlayer state1
            , currentMarble = nextM
            , nextAvailableMarble = (nextM + 1)
            }

clockwiseFrom :: GameState -> Marble -> [Marble]
clockwiseFrom state startM = let
  successor = ((clockwise state)!startM)
  in startM:(clockwiseFrom state successor)

counterCWFrom :: GameState -> Marble -> [Marble]
counterCWFrom state startM = let
  successor = ((counterCW state)!startM)
  in startM:(counterCWFrom state successor)

play23Move :: GameState -> GameState
play23Move state0 = let
  GameState _ _ curM nextM nextP numP sc = state0
  [newCurrent, toRemove] = take 2 $ drop 6 $ counterCWFrom state0 curM
  points = toRemove + nextM
  newScores = Map.adjust (+ points) nextP sc
  state1 = delete state0 toRemove
  in state1 { nextPlayer = findNextPlayer state1
            , currentMarble = newCurrent
            , nextAvailableMarble = (nextM + 1)
            , scores = newScores
            }

playMove :: GameState -> GameState
playMove state = case ((nextAvailableMarble state) `mod` 23) of
                   0 -> play23Move state
                   _ -> playNormalMove state 

highScore :: GameState -> Int
highScore state = maximum $ Map.elems $ scores state

playGame :: Int -> Marble -> GameState
playGame players highMarble = let
  finalState = playGame0 (initialState players) highMarble
  in finalState

playGame0 :: GameState -> Marble -> GameState
playGame0 state highMarble =
  if (nextAvailableMarble state > highMarble)
    then state
    else let nextState = playMove $! state
         in playGame0 nextState highMarble

solveDay9 :: Int -> Marble -> Int
solveDay9 players highMarble = highScore $ playGame players highMarble

main :: IO ()
main = do
  putStrLn $ show $ solveDay9 9 25
  putStrLn $ show $ solveDay9 10 1618
  putStrLn $ show $ solveDay9 13 7999
  putStrLn $ show $ solveDay9 17 1104
  putStrLn $ show $ solveDay9 21 6111
  putStrLn $ show $ solveDay9 30 5807
  putStrLn $ show $ solveDay9 464 71730
  putStrLn $ show $ solveDay9 464 7173000
