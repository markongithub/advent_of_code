module Day19 where

import Common
import Data.Map (Map)
import qualified Data.Map as Map

data TempRule = RealRule Rule | TempDisjunction TempRule TempRule
              | TempConcat [Int] deriving (Eq, Show)

data Rule = StringMatch String | Disjunction Rule Rule | Concat [Rule]
            deriving (Eq, Show)

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

flattenRuleByNumber :: Map Int TempRule -> Int -> Rule
flattenRuleByNumber m i = case (Map.lookup i m) of
  Nothing -> error "rule not found in table"
  Just r -> flattenTempRule m r

flattenTempRule :: Map Int TempRule -> TempRule -> Rule
flattenTempRule _ (RealRule r) = r
flattenTempRule m (TempDisjunction a b) = Disjunction (flattenTempRule m a) (flattenTempRule m b)
flattenTempRule m (TempConcat ls) = flattenConcat $ Concat (map (flattenRuleByNumber m) ls)

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

