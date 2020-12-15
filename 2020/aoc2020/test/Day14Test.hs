import Data.List (sort)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day14

day14TestData1 = [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                 , "mem[8] = 11"
                 , "mem[7] = 101"
                 , "mem[8] = 0"
                 ]

day14TestData2 = [ "mask = 000000000000000000000000000000X1001X"
                 , "mem[42] = 100"
                 , "mask = 00000000000000000000000000000000X0XX"
                 , "mem[26] = 1"
                 ]

testMask = drop 7 $ head day14TestData2

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day14PureTests = [ makeTest 165 (solvePart1Pure day14TestData1)
                 , makeTest "010" (replaceAll "X1X" 'X' '0')
                 , makeTest [0,2] (expandXes "0X0")
                 , makeTest "101010" (showBinary 42)
                 , makeTest "00101010" (showBinaryWithPadding 42 8)
                 , makeTest (sort [26,27,58,59])
                     (sort $ maskAddresses 42 testMask)
                 , makeTest (sort [0,2,8,10]) (sort (expandXes "X0X0"))
                 , makeTest 208 (solvePart2Pure day14TestData2)
                 ]

main = do
  day14Solution1 <- solvePart1
  let day14Solution1Test = testCase [] (assertEqual [] 8570568288597 
                                        day14Solution1)
  day14Solution2 <- solvePart2
  let day14Solution2Test = testCase [] (assertEqual [] 3289441921203
                                        day14Solution2)
  defaultMain $ testGroup []
    (day14PureTests ++ [day14Solution1Test, day14Solution2Test])
