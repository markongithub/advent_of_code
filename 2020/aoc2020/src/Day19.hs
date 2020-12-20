module Day19 where

import Common
import Data.Map (Map)
import qualified Data.Map as Map

data TempRule = RealRule Rule | TempDisjunction TempRule TempRule
              | TempConcat [Int] deriving (Eq, Show)

data Rule = StringMatch String | Disjunction Rule Rule | Concat [Rule]
          | InfiniteInfix Rule Rule | ZeroOrMore Rule  deriving (Eq, Show)

parseRule :: String -> (Int, TempRule)
parseRule s = let
  ruleIDStr = takeWhile (/= ':') s
  ruleID = read ruleIDStr :: Int
  restOfRule = drop (length ruleIDStr + 2) s
  rule = case restOfRule of
    ('"':xs) -> RealRule $ StringMatch (init xs)
    other -> case (parseDisjunction other) of
      Nothing -> parseConcat other
      Just (a, b) -> TempDisjunction (parseConcat a) (parseConcat b)
  in (ruleID, rule)

parseDisjunction :: String -> Maybe (String, String)
parseDisjunction s = let
  fromPipe = dropWhile (/= '|') s
  in case fromPipe of
    [] -> Nothing
    fromPipeStr -> Just (init (takeWhile (/= '|') s), tail fromPipeStr)

parseConcat :: String -> TempRule
parseConcat str = let
  substrings = splitOnSeparator ' ' str
  ints = map (\s -> read s :: Int) substrings
  in TempConcat ints

parseRules0 :: [String] -> Map Int TempRule -> Map Int TempRule
parseRules0 [] oldMap = oldMap
parseRules0 (x:xs) oldMap = let
  (ruleID, rule) = parseRule x
  newMap = Map.insert ruleID rule oldMap
  in parseRules0 xs newMap

parseRules :: [String] -> Map Int TempRule
parseRules ls = parseRules0 ls Map.empty

flattenRuleByNumber :: Map Int TempRule -> Bool -> Int -> Rule
flattenRuleByNumber m True 11 = InfiniteInfix (flattenRuleByNumber m True 42) (flattenRuleByNumber m True 31)
flattenRuleByNumber m True 8 = Concat [flattenRuleByNumber m True 42, ZeroOrMore (flattenRuleByNumber m True 42)]
flattenRuleByNumber m part2 i = case (Map.lookup i m) of
  Nothing -> error "rule not found in table"
  Just r -> flattenTempRule m part2 r

flattenTempRule :: Map Int TempRule -> Bool -> TempRule -> Rule
flattenTempRule _ _ (RealRule r) = r
flattenTempRule m part2 (TempDisjunction a b) = Disjunction (flattenTempRule m part2 a) (flattenTempRule m part2 b)
flattenTempRule m part2 (TempConcat ls) = flattenConcat $ Concat (map (flattenRuleByNumber m part2) ls)

flattenConcat :: Rule -> Rule
flattenConcat (StringMatch s) = StringMatch s
flattenConcat (Disjunction a b) = Disjunction a b
flattenConcat (Concat []) = StringMatch ""
flattenConcat (Concat ls) = let
  newList = map flattenConcat ls
  in case newList of
    [x] -> x
    (StringMatch s1:StringMatch s2:ys) -> flattenConcat $ Concat (StringMatch (s1 ++ s2):ys)
    ls -> Concat ls

applyRule0 :: Rule -> String -> Maybe String
applyRule0 (StringMatch s1) s2 = let
  substring = take (length s1) s2
  in if s1 == substring then Just (drop (length s1) s2) else Nothing
applyRule0 (Disjunction a b) s = case (applyRule0 a s) of
  Just remainder -> Just remainder
  Nothing -> applyRule0 b s
applyRule0 (InfiniteInfix a b) s = applyInfiniteInfix a b s
applyRule0 (Concat []) s = Just s
applyRule0 (Concat (r:rs)) s = case (applyRule0 r s) of
  Nothing -> Nothing
  Just remainder -> applyRule0 (Concat rs) remainder

applyRule :: Rule -> String -> Bool
applyRule r s = case (applyRule0 r s) of
  Just [] -> True
  _       -> False

applyInfiniteInfix :: Rule -> Rule -> String -> Maybe String
applyInfiniteInfix a b s = case (applyRule0 a s) of
  Nothing -> Nothing
  Just rem1 -> case (applyInfiniteInfix a b rem1) of
    Nothing -> applyRule0 b rem1
    Just rem2 -> applyRule0 b rem2

applyZeroOrMore :: Rule -> String -> Maybe String
applyZeroOrMore rule s = case (applyRule0 rule s) of
  Nothing -> Just s
  Just rem1 -> applyZeroOrMore rule rem1

parseInput :: Bool -> [String] -> (Rule, [String])
parseInput part2 strings = let
  ruleStrings = takeWhile (not . null) strings
  testStrings = drop (length ruleStrings + 1) strings
  rule = flattenRuleByNumber (parseRules ruleStrings) part2 0
  in (rule, testStrings)

solvePart1Func :: Rule -> [String] -> Int
solvePart1Func rule ls = length $ filter (applyRule rule) ls

solvePart1 :: String -> IO Int
solvePart1 filename = do
  text <- readFile filename
  let (rule, input) = parseInput False $ lines text
  return $ solvePart1Func rule input
