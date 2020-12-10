import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day01

day1TestData = [1721,979,366,299,675,1456]

day1Test = testCase []
  (assertEqual [] 514579 (productOf2020Sums day1TestData))

allTests = [day1Test]

main = defaultMain $ testGroup [] allTests
