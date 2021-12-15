module Day15Test (day15Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day15

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day15PureTests = [
    makeTest 40 test1Output
  , makeTest 621 part1Output
  ]

day15Tests = day15PureTests
