module Day21Test (day21Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day21

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day21PureTests = [
  ]

day21Tests = day21PureTests
