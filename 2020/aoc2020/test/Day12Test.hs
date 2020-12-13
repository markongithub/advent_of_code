import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day12

day12TestData = ["F10", "N3", "F7", "R90", "F11"]

day12Test = testCase []
  (assertEqual [] 25 (solvePart1Pure day12TestData))

main = do
  day12Solution1 <- solvePart1
  let day12Solution1Test = testCase [] (assertEqual [] 796 day12Solution1)
  defaultMain $ testGroup [] [day12Test, day12Solution1Test]
