module Day16Test (day16Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day16

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day16PureTests = [
    makeTest (Literal 9 2021) (fst $ parseLiteral 9 $ binStringToBits "101111111000101000")
  ]

day16Tests = day16PureTests
