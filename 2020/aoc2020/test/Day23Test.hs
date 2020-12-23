import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Day23

makeTest expected actual = testCase [] (assertEqual [] expected actual)

initialTestState = fromList [3,8,9,1,2,5,4,6,7]
longTestState = fromList $ initialLongList [3,8,9,1,2,5,4,6,7]

day23PureTests = [
    makeTest (fromList [2,8,9,1,5,4,6,7,3]) (playTurn initialTestState)
  , makeTest (GameState [3,8,9,1,2,5,4,6,7] 1 9) initialTestState
  , makeTest [2,1,9,8,7,6,5,4] (destinationCandidates initialTestState)
  , makeTest "54673289" (labelsAfter1 $ fromList [2,8,9,1,5,4,6,7,3])
  , makeTest (fromList [8,3,7,4,1,9,2,6,5]) (playNTurns initialTestState 10)
  , makeTest "74698532" solvePart1
  , makeTest 1000000 (length $ initialLongList part1Input)
  , makeTest (2,5) (twoCupsAfter1 initialTestState)
  , makeTest 10 (part2Product initialTestState)
  , makeTest 10 (part2Product $ fromList [5,4,6,7,3,8,9,1,2])
  , makeTest (5,4) (twoCupsAfter1 (playTurn longTestState))
  , makeTest (3,2) (twoCupsAfter1 (playNTurns longTestState 2))
  , makeTest (3,2) (twoCupsAfter1 (playNTurns longTestState 100))
  -- , makeTest 5556969 solvePart2
  ]

main = do
--  day23Solution1 <- solvePart1
--  let day23Solution1Test = testCase [] (assertEqual [] 2436
--                                        day23Solution1)
--  day23Solution2 <- solvePart2
--  let day23Solution2Test =
--        testCase [] (
--          assertEqual [] "dhfng,pgblcd,xhkdc,ghlzj,dstct,nqbnmzx,ntggc,znrzgs"
--            day23Solution2)
  defaultMain $ testGroup [] (day23PureTests) --  ++ [
--    day23Solution1Test, day23Solution2Test])
