import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day25

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day25PureTests = [
    makeTest 8 (findLoopSize 7 20201227 5764801)
  , makeTest 11 (findLoopSize 7 20201227 17807724)
  , makeTest 7 (transform 3 10 3)
  , makeTest 6 (transform 2 10 8)
  , makeTest 5764801 (transform 7 20201227 8)
  , makeTest 17807724 (transform 7 20201227 11)
  , makeTest 14897079 (transform 17807724 20201227 8)
  , makeTest 14897079 (transform 5764801 20201227 11)
  , makeTest 14897079 (transform 5764801 20201227 11)
  , makeTest 18293391 solvePart1
  ]

main = do
  defaultMain $ testGroup [] day25PureTests
