import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Data.Map as Map

import Day23

makeTest expected actual = testCase [] (assertEqual [] expected actual)

sameCups expected state  = testCase [] (assertEqual [] expected (cupsInOrder state))

initialTestState :: GameState
initialTestState = fromList [3,8,9,1,2,5,4,6,7]
longTestState = fromList $ initialLongList [3,8,9,1,2,5,4,6,7]

day23PureTests = [
    sameCups [3,8,9,1,2,5,4,6,7] initialTestState
  , sameCups [2,8,9,1,5,4,6,7,3] (playTurn initialTestState)
  , makeTest (GameState 3 (Map.fromList [(1,2),(2,5),(3,8),(4,6),(5,4),(6,7),(7,3),(8,9),(9,1)]) 1 9) initialTestState
  , makeTest "54673289" (labelsAfter1 $ fromList [2,8,9,1,5,4,6,7,3])
  , sameCups [8,3,7,4,1,9,2,6,5] (playNTurns initialTestState 10)
  , makeTest "74698532" solvePart1
  , makeTest 1000000 (length $ initialLongList part1Input)
  , makeTest (2,5) (twoCupsAfter1 initialTestState)
  , makeTest 10 (part2Product initialTestState)
  , makeTest 10 (part2Product $ fromList [5,4,6,7,3,8,9,1,2])
  , makeTest (5,4) (twoCupsAfter1 (playTurn longTestState))
  , makeTest (3,2) (twoCupsAfter1 (playNTurns longTestState 2))
  , makeTest 286194102744 solvePart2
  ]

main = do
  defaultMain $ testGroup [] day23PureTests
