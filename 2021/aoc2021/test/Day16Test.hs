module Day16Test (day16Tests) where

import Test.Tasty.HUnit (assertEqual, testCase)

import Day16

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day16PureTests = [
    makeTest (Literal 9 2021)
      (fst $ parseLiteral 9 $ binStringToBits "101111111000101000")
  , makeTest (Literal 6 2021) (parseBitString "110100101111111000101000")
  , makeTest (Operation 1 6 [Literal 6 10,Literal 2 20])
      (parseBitString "00111000000000000110111101000101001010010001001000000000")
  , makeTest (Operation 7 3  [Literal 2 1,Literal 4 2,Literal 1 3])
      (parseBitString "11101110000000001101010000001100100000100011000001100000")
  , makeTest 16 (part1Hex "8A004A801A8002F478")
  , makeTest 12 (part1Hex "620080001611562C8802118E34")
  , makeTest 23 (part1Hex "C0015000016115A2E0802F182340")
  , makeTest 31 (part1Hex "A0016C880162017C3686B18A3D4780")
  , makeTest 860 solvePart1
  ]

day16Tests = day16PureTests
