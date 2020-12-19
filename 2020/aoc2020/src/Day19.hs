module Day19 where

import Common
import Data.Map (Map)
import qualified Data.Map as Map

data TempRule = RealRule Rule | TempDisjunction TempRule TempRule
              | TempConcatenation [Int] deriving (Eq, Show)

data Rule = StringMatch String | Disjunction Rule Rule | Concatenation Rule Rule
            deriving (Eq, Show)

parseRule :: String -> (Int, TempRule)
parseRule s = let
  ruleIDStr = takeWhile (/= ':') s
  ruleID = read ruleIDStr :: Int
  restOfRule = drop (length ruleIDStr + 2) s
  rule = case restOfRule of
    ('"':xs) -> RealRule $ StringMatch (init xs)
    other -> case (parseDisjunction other) of
      Nothing -> parseConcatenation other
      Just (a, b) -> TempDisjunction (parseConcatenation a) (parseConcatenation b)
  in (ruleID, rule)

parseDisjunction :: String -> Maybe (String, String)
parseDisjunction s = let
  fromPipe = dropWhile (/= '|') s
  in case fromPipe of
    [] -> Nothing
    fromPipeStr -> Just (init (takeWhile (/= '|') s), tail fromPipeStr)

parseConcatenation :: String -> TempRule
parseConcatenation str = let
  substrings = splitOnSeparator ' ' str
  ints = map (\s -> read s :: Int) substrings
  in TempConcatenation ints

