import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day22

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day21PureTests = [ makeTest 306 (solvePart1Func (testDeck1, testDeck2))
                 ]

main = do
  day21Solution1 <- solvePart1
  let day21Solution1Test = testCase [] (assertEqual [] 5556969
                                        day21Solution1)
--  day21Solution2 <- solvePart2
--  let day21Solution2Test =
--        testCase [] (
--          assertEqual [] "dhfng,pgblcd,xhkdc,ghlzj,dstct,nqbnmzx,ntggc,znrzgs"
--            day21Solution2)
  defaultMain $ testGroup [] (day21PureTests ++ [
    day21Solution1Test]) -- , day21Solution2Test])
