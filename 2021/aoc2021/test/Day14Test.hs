module Day14Test (day14Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day14

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day14PureTests = [
    makeTest 1588 test1Output
  , makeTest 4244 solvePart1
--  , makeTest 4244 solvePart2
  ]

day14Tests = day14PureTests
