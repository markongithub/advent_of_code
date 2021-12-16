module Day15Test (day15Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day15

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day15PureTests = [
    makeTest 40 test1Output
--  , makeTest 621 part1Output
  , makeTest 6 (lookupCoords test2Grid (49,0))
  , makeTest 315 (solvePart1 test2Grid)
  ]

day15Tests = day15PureTests
