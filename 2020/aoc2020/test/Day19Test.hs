import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day19

day19TestRules = ["0: 4 1 5", "1: 2 3 | 3 2", "2: 4 4 | 5 5", "3: 4 5 | 5 4","4: \"a\"","5: \"b\""]
day19TestRulesExpected = Concat [
  StringMatch "a",
  Disjunction
    (Concat [Disjunction (StringMatch "aa") (StringMatch "bb"),
             Disjunction (StringMatch "ab") (StringMatch "ba")])
    (Concat [Disjunction (StringMatch "ab") (StringMatch "ba"),
             Disjunction (StringMatch "aa") (StringMatch "bb")]),
  StringMatch "b"]
day19TestRulesActual = flattenRuleByNumber (parseRules day19TestRules) False 0

day19GoodTestInputs = ["aaaabb", "aaabab", "abbabb", "abbbab", "aabaab", "aabbbb", "abaaab", "ababbb"]

(rule8, rule11, _) = parseInput2
  ["0: 8 11", "8: \"unused\"", "11: \"unused\"", "31: \"b\"", "42: \"a\""]

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day19PureTests = [ makeTest (5, TempConcat [72, 58]) (parseRule "5: 72 58")
                 , makeTest (128, TempDisjunction (TempConcat [66, 58]) (TempConcat [57, 72])) (parseRule "128: 66 58 | 57 72")
                 , makeTest (58, RealRule (StringMatch "b")) (parseRule "58: \"b\"")
                 , makeTest day19TestRulesExpected day19TestRulesActual
                 , makeTest True (applyRule (StringMatch "m") "m")
                 , makeTest True (applyRule (StringMatch "mark") "mark")
                 , makeTest True (applyRule (
                     Concat [StringMatch "mark", StringMatch "sucks"])
                     "marksucks")
                 , makeTest True (applyRule (
                     Concat [
                       StringMatch "mark",
                       Disjunction (StringMatch "sucks") (StringMatch "rules")])
                     "marksucks")
                 , makeTest True (applyRule (
                     Concat [
                       StringMatch "mark",
                       Disjunction (StringMatch "sucks") (StringMatch "rules")])
                     "markrules")
                 , makeTest False (applyRule (
                     Concat [
                       StringMatch "mark",
                       Disjunction (StringMatch "sucks") (StringMatch "rules")])
                     "markrulestoo")
                 , makeTest True (applyRule (
                     Concat [
                       StringMatch "mark",
                       ZeroOrMore (StringMatch "really"),
                       StringMatch "sucks"])
                     "marksucks")
                 , makeTest True (applyRule (
                     InfiniteInfix
                       (StringMatch "mark")
                       (StringMatch "sucks"))
                     "marksucks")
                 , makeTest True (applyRule (
                     InfiniteInfix
                       (StringMatch "mark")
                       (StringMatch "sucks"))
                     "markmarksuckssucks")
                 , makeTest True (applyRule (
                     Concat [
                       StringMatch "mark",
                       Disjunction (StringMatch "sucks") (StringMatch "rules"),
                       Disjunction (StringMatch "too") (StringMatch "also")])
                     "markrulestoo")
                 , makeTest (take 8 (repeat True)) (map (applyRule day19TestRulesActual) day19GoodTestInputs)
                 , makeTest 2 (solvePart1Func day19TestRulesActual
                                ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"])
                 , makeTest [True, False, True, False, False]
                     (map (applyRule day19TestRulesActual)
                        ["ababbb", "bababa", "abbbab", "aaabbb", "aaaabbb"])
                 , makeTest True (applyPart2 rule8 rule11 "aaaaaabb")
                 , makeTest True (applyPart2 rule8 rule11 "aaabb")
                 ]

main = do
  day19Solution1 <- solvePart1 "data/input19.txt"
  let day19Solution1Test = testCase [] (assertEqual [] 142
                                        day19Solution1)
  part2Example1 <- solvePart1 "data/input19a.txt"
  let part2Test1 = makeTest 3 part2Example1
  part2Example2 <- solvePart2 "data/input19a.txt"
  let part2Test2 = makeTest 12 part2Example2
  day19Solution2 <- solvePart2 "data/input19.txt"
  let day19Solution2Test = testCase [] (assertEqual [] 294
                                        day19Solution2)
  defaultMain $ testGroup [] (day19PureTests ++ [
    day19Solution1Test, part2Test1, part2Test2, day19Solution2Test])
