module Day12Test (day12Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day12

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day12PureTests = [
    makeTest 10 (countPaths graph1 Start False)
  , makeTest 226 (countPaths graph3 Start False)
  , makeTest 3779 solvePart1
  , makeTest 36 (countPaths graph1 Start True)
  , makeTest 3509 (countPaths graph3 Start True)
  , makeTest 96988 solvePart2
  ]

day12Tests = day12PureTests
