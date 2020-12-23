import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Day23

makeTest expected actual = testCase [] (assertEqual [] expected actual)

initialTestState = GameState [3,8,9,1,2,5,4,6,7]
day23PureTests = [ makeTest (GameState [2,8,9,1,5,4,6,7,3]) (playTurn initialTestState)
                 , makeTest "54673289" (labelsAfter1 $ GameState [2,8,9,1,5,4,6,7,3])
                 , makeTest (GameState [8,3,7,4,1,9,2,6,5]) (playNTurns initialTestState 10)
                 , makeTest "5556969" solvePart1
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
