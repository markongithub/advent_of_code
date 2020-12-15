import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day15

day15TestData1 = "0,3,6"

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day15PureTests = [ makeTest 436 (solvePart1Func day15TestData1)
                 , makeTest [0,3,6,0,3,3,1,0,4,0] (take 10 $ spokenNumbersFromInput day15TestData1)
                 , makeTest 4 (nthWord 9 day15TestData1)
                 , makeTest 536 solvePart1
                 , makeTest 175594 (solvePart2Func day15TestData1)
                 , makeTest 2578 (solvePart2Func "1,3,2")
--                 , makeTest 536 solvePart2
                 ]

main = defaultMain $ testGroup [] day15PureTests
