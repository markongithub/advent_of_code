import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day19

day19TestRules = ["0: 4 1 5", "1: 2 3 | 3 2", "2: 4 4 | 5 5", "3: 4 5 | 5 4","4: \"a\"","5: \"b\""]
day19TestRulesParsed = Concat [StringMatch "a",Disjunction (Concat [Disjunction (StringMatch "aa") (StringMatch "bb"),Disjunction (StringMatch "ab") (StringMatch "ba")]) (Concat [Disjunction (StringMatch "ab") (StringMatch "ba"),Disjunction (StringMatch "aa") (StringMatch "bb")]),StringMatch "b"]

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day19PureTests = [ makeTest (5, TempConcat [72, 58]) (parseRule "5: 72 58")
                 , makeTest (128, TempDisjunction (TempConcat [66, 58]) (TempConcat [57, 72])) (parseRule "128: 66 58 | 57 72")
                 , makeTest (58, RealRule (StringMatch "b")) (parseRule "58: \"b\"")
                 , makeTest day19TestRulesParsed (flattenRuleByNumber (parseRules day19TestRules) 0)
                 ]



main = defaultMain $ testGroup [] day19PureTests
