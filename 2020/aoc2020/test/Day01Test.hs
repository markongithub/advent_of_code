import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day01

day1TestData = [1721,979,366,299,675,1456]

day1Test = testCase []
  (assertEqual [] 514579 (productOf2020Sums day1TestData))

day1Test2 = testCase []
  (assertEqual [] 241861950 (productOf2020TripleSum day1TestData))

main = do
  day1Solution1 <- solvePart1
  let day1SolutionTest = testCase [] (assertEqual [] 55776 day1Solution1)
  day1Solution2 <- solvePart2
  let day1Solution2Test = testCase [] (assertEqual [] 223162626 day1Solution2)
  defaultMain $ testGroup []
    [day1Test, day1SolutionTest, day1Test2, day1Solution2Test]
