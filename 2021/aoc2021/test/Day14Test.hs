module Day14Test (day14Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day14

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day14PureTests = [
    makeTest "NCNBCHB" (applyRules test1Rules test1Template)
  , makeTest "NCNBCHB" (applySteps test1Rules test1Template 1)
  , makeTest "NBCCNBBBCBHCB" (applySteps test1Rules test1Template 2)
  , makeTest "NBBBCNCCNBBNBNBBCHBHHBCHB" (applySteps test1Rules test1Template 3)
  , makeTest "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB" (applySteps test1Rules test1Template 4)
  , makeTest 1588 test1Output
  , makeTest 4244 solvePart1
  ]

day14Tests = day14PureTests
