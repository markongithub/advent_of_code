import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day15

day15TestData1 = "0,3,6"

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day15PureTests = [ makeTest 436 (solvePart1Func day15TestData1)
                 , makeTest [0,3,6,0,3,3,1,0,4,0] (take 10 $ spokenNumbersFromInput day15TestData1)
                 , makeTest 536 solvePart1
                 ]

main = defaultMain $ testGroup [] day15PureTests
