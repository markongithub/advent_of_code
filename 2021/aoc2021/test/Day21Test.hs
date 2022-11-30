module Day21Test (day21Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day21

makeTest expected actual = testCase [] (assertEqual [] expected actual)

testStart = GameState 4 8 0 0 0 True

day21PureTests = [
    makeTest (GameState 0 8 10 0 3 False) (applyRollSet testStart 6)
  , makeTest (GameState 6 6 26 22 24 True) (applyRollsUntilEnd testStart [1..24])
  , makeTest (GameState 4 8 4 8 6 True) (applyRollsUntilEnd testStart (take 6 (repeat 10)))
  , makeTest (GameState 4 8 16 32 24 True) (applyRollsUntilEnd testStart (take 24 (repeat 10)))
  , makeTest (GameState 4 8 160 320 240 True) (applyRollsUntilEnd testStart (take 240 (repeat 10)))
  , makeTest (GameState 4 8 480 960 720 True) (applyRollsUntilEnd testStart (take 720 (repeat 10)))
  , makeTest (GameState 4 8 500 1000 750 True) (applyRollsUntilEnd testStart (take 750 (repeat 10)))
  , makeTest (GameState 4 8 500 1000 750 True) (applyRollsUntilEnd testStart (take 10000 (repeat 10)))
  , makeTest (GameState 0 3 1000 745 993 False) (applyRollsUntilEnd testStart (concat $ repeat [1..100]))
  , makeTest 739785 part1Test
  , makeTest 605070 solvePart1
  , makeTest 444356092776315 part2Test
  , makeTest 218433063958910 solvePart2
  ]

day21Tests = day21PureTests
