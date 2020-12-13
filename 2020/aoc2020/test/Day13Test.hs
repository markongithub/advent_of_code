import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day13

day13TestTimestamp = "939"
day13TestBusLine = "7,13,x,x,59,x,31,19"

day13Test1 = testCase []
  (assertEqual [] 295 (solvePart1Pure day13TestTimestamp day13TestBusLine))

main = do
  day13Solution1 <- solvePart1
  let day13Solution1Test = testCase [] (assertEqual [] 3966 day13Solution1)
--  day13Solution2 <- solvePart2
--  let day13Solution2Test = testCase [] (assertEqual [] 39446 day13Solution2)
  defaultMain $ testGroup []
    [day13Test1, day13Solution1Test] -- , day13Test2, day13Solution2Test]
