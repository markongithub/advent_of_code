import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day12

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day12PureTests = [
    makeTest 10 (countPaths graph1 (SmallCave "start"))
  ]

main = do
  defaultMain $ testGroup [] day12PureTests
