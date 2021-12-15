module Day14 where

import Data.Map (Map, (!))
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

type FrequencyMap = Map Char Int
type PairCache = Map (Char, Char, Int) FrequencyMap

applyDown :: RuleSet -> Char -> Char -> Int -> PairCache
             -> (PairCache, FrequencyMap)
applyDown rules x y steps cache
  | steps == 0                     = (cache, singleton)
  | Map.member cacheKey cache = (cache, cache!cacheKey)
  | not (Map.member (x, y) rules)  = (cacheWithSingleton, singleton)
  | otherwise                      = (finalCache, finalMap)
  where
    cacheKey = (x, y, steps)
    singleton = Map.singleton x 1
    cacheWithSingleton = Map.insert (x, y, steps) singleton cache
    newChar = rules!(x, y)
    (cacheFromLeft, leftMap) = applyDown rules x newChar (steps - 1) cache
    (cacheFromRight, rightMap) = applyDown rules newChar y (steps - 1) cacheFromLeft
    finalMap = Map.unionWith (+) leftMap rightMap
    finalCache = Map.insert cacheKey finalMap cacheFromRight

applyAcross :: String -> RuleSet -> Int -> PairCache -> FrequencyMap -> FrequencyMap
applyAcross [] _ _ _ _ = error "I thought this wouldn't happen in applyAcross"
applyAcross [c] _ _ _ accu = Map.insertWith (+) c 1 accu
applyAcross (x:(y:ys)) rules steps cache accu
  | otherwise = applyAcross (y:ys) rules steps newCache newAccu
  where
    (newCache, thisMap) = applyDown rules x y steps cache
    newAccu = Map.unionWith (+) accu thisMap

applySteps :: RuleSet -> String -> Int -> Map Char Int
applySteps rules s steps = applyAcross s rules steps Map.empty Map.empty

part2Problem :: String -> RuleSet -> Int
part2Problem s rs = part1Difference $ applySteps rs s 40

test2Output = part2Problem test1Template test1Rules

solvePart2 = part2Problem inputTemplate inputRules
