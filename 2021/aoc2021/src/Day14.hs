module Day14 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

type Rule = ((Char, Char), Char)
type RuleSet = Map (Char, Char) Char

applyRules :: RuleSet -> String -> String
applyRules _ [] = []
applyRules _ [x] = [x]
applyRules rules (x:(y:ys)) = case (Map.lookup (x, y) rules) of
  Nothing -> x:(applyRules rules (y:ys))
  Just c  -> x:(c:(applyRules rules (y:ys)))

applySteps :: RuleSet -> String -> Int -> String
applySteps rs s steps = let
  f = applyRules rs
  outputs = iterate f s
  in last $ take (steps + 1) outputs

parseRule :: String -> Rule
parseRule s = ((s!!0, s!!1), (s!!6))

parseRuleSet :: [String] -> RuleSet
parseRuleSet strs = Map.fromList $ map parseRule strs

countElements :: (Ord a) => [a] -> Map a Int
countElements xs = let
  increment :: (Ord b) => Map b Int -> b -> Map b Int
  increment m k = Map.insertWith (+) k 1 m
  in foldl increment Map.empty xs

part1Difference :: String -> Int
part1Difference s = let
  orderedCounts = sort $ Map.elems $ countElements s
  in (last orderedCounts) - (head orderedCounts)

part1Problem :: String -> RuleSet -> Int
part1Problem s rs = part1Difference $ applySteps rs s 10

test1Rules = parseRuleSet [
    "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]

test1Template = "NNCB"

test1Output = part1Problem test1Template test1Rules
