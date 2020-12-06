import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day01

checkSum :: (Int, String) -> TestTree
checkSum (expected, input) = testCase []
  (assertEqual [] expected (sumDigits input))

allTests = map checkSum [(6, "123")]

main = defaultMain $ testGroup [] allTests
