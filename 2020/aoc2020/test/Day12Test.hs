import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day12

day12TestData = lines "F10\
\N3\
\F7\
\R90\
\F11"

day12Test = testCase []
  (assertEqual [] 0 0)

main = do
--  day12Solution1 <- solvePart1
--  let day12SolutionTest = testCase [] (assertEqual [] 55776 day12Solution1)
  defaultMain $ testGroup []
    [day12Test]
