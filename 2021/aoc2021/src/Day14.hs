module Day14 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

type Rule = ((Char, Char), Char)
type RuleSet = Map (Char, Char) Char

parseRule :: String -> Rule
parseRule s = ((s!!0, s!!1), (s!!6))

parseRuleSet :: [String] -> RuleSet
parseRuleSet strs = Map.fromList $ map parseRule strs

part1Difference :: Ord a => Map a Int -> Int
part1Difference m = let
  orderedCounts = sort $ Map.elems $ m
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

inputTemplate = "HBHVVNPCNFPSVKBPPCBH"

inputRules = parseRuleSet [
    "HV -> B"
  , "KS -> F"
  , "NH -> P"
  , "OP -> K"
  , "OV -> C"
  , "HN -> O"
  , "FF -> K"
  , "CP -> O"
  , "NV -> F"
  , "VB -> C"
  , "KC -> F"
  , "CS -> H"
  , "VC -> F"
  , "HF -> V"
  , "NK -> H"
  , "CF -> O"
  , "HH -> P"
  , "FP -> O"
  , "OH -> K"
  , "NN -> C"
  , "VK -> V"
  , "FB -> F"
  , "VP -> N"
  , "FC -> P"
  , "SV -> F"
  , "NO -> C"
  , "VN -> S"
  , "CH -> N"
  , "FN -> N"
  , "FV -> P"
  , "CN -> H"
  , "PS -> S"
  , "VF -> K"
  , "BN -> S"
  , "FK -> C"
  , "BB -> H"
  , "VO -> P"
  , "KN -> N"
  , "ON -> C"
  , "BO -> S"
  , "VS -> O"
  , "PK -> C"
  , "SK -> P"
  , "KF -> K"
  , "CK -> O"
  , "PB -> H"
  , "PF -> O"
  , "KB -> V"
  , "CC -> K"
  , "OK -> B"
  , "CV -> P"
  , "PO -> O"
  , "SH -> O"
  , "NP -> F"
  , "CO -> F"
  , "SS -> P"
  , "FO -> K"
  , "NS -> O"
  , "PN -> H"
  , "PV -> V"
  , "KP -> C"
  , "BK -> B"
  , "BP -> F"
  , "NB -> C"
  , "OF -> O"
  , "OC -> O"
  , "HO -> C"
  , "SC -> K"
  , "HC -> C"
  , "HS -> B"
  , "KH -> N"
  , "FS -> N"
  , "PH -> O"
  , "PC -> V"
  , "BS -> O"
  , "KO -> F"
  , "SP -> K"
  , "OB -> O"
  , "SF -> K"
  , "KV -> F"
  , "NC -> B"
  , "SO -> C"
  , "CB -> S"
  , "VH -> V"
  , "FH -> F"
  , "SN -> V"
  , "SB -> P"
  , "PP -> B"
  , "BF -> K"
  , "HB -> O"
  , "OO -> V"
  , "HP -> H"
  , "KK -> O"
  , "BV -> K"
  , "BH -> B"
  , "HK -> H"
  , "BC -> C"
  , "VV -> S"
  , "OS -> F"
  , "NF -> B"
  ]

solvePart1 = part1Problem inputTemplate inputRules

applyDown :: RuleSet -> Char -> Char -> Int -> Map Char Int
applyDown rules x y 0 = Map.singleton x 1
applyDown rules x y steps = case (Map.lookup (x, y) rules) of
  Nothing -> Map.singleton x 1
  Just newChar -> let
    downLeft = applyDown rules x newChar (steps - 1)
    downRight = applyDown rules newChar y (steps - 1)
    in Map.unionWith (+) downLeft downRight

applyAcross :: String -> RuleSet -> Int -> Map Char Int -> Map Char Int
applyAcross [] _ _ _ = error "I thought this wouldn't happen in applyAcross"
applyAcross [c] _ _ accu = Map.insertWith (+) c 1 accu
applyAcross (x:(y:ys)) rules steps accu = let
  newAccu = Map.unionWith (+) accu $ applyDown rules x y steps
  in applyAcross (y:ys) rules steps newAccu

applySteps :: RuleSet -> String -> Int -> Map Char Int
applySteps rules s steps = applyAcross s rules steps Map.empty

part2Problem :: String -> RuleSet -> Int
part2Problem s rs = part1Difference $ applySteps rs s 40

solvePart2 = part2Problem inputTemplate inputRules
