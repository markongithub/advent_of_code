import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day19

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day19PureTests = [ makeTest (5, TempConcatenation [72, 58]) (parseRule "5: 72 58")
                 , makeTest (128, TempDisjunction (TempConcatenation [66, 58]) (TempConcatenation [57, 72])) (parseRule "128: 66 58 | 57 72")
                 , makeTest (58, RealRule (StringMatch "b")) (parseRule "58: \"b\"")
                 ]

main = defaultMain $ testGroup [] day19PureTests
