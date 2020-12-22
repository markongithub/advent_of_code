import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day22

makeTest expected actual = testCase [] (assertEqual [] expected actual)

infiniteLoopDecks = (Queue [43,19] [], Queue [2,29,14] [])
day21PureTests = [ makeTest 306 (solvePart1Func (testDeck1, testDeck2))
                 , makeTest 105 (solvePart2Func infiniteLoopDecks)
                 , makeTest 291 (solvePart2Func (testDeck1, testDeck2))
                 ]

main = do
  day21Solution1 <- solvePart1
  let day21Solution1Test = testCase [] (assertEqual [] 33694
                                        day21Solution1)
  day21Solution2 <- solvePart2
  let day21Solution2Test = testCase [] (assertEqual [] 31835
                                        day21Solution2)
--  day21Solution2 <- solvePart2
--  let day21Solution2Test =
--        testCase [] (
--          assertEqual [] "dhfng,pgblcd,xhkdc,ghlzj,dstct,nqbnmzx,ntggc,znrzgs"
--            day21Solution2)
  defaultMain $ testGroup [] (day21PureTests ++ [
    day21Solution1Test, day21Solution2Test])
