module Days.Day19 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative ((<|>))
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
branchParser :: Parser Rule
branchParser = do
  firstVar <- anyChar
  operator <- satisfy (inClass "<>")
  comparator <- many1 digit
  char ':'
  branchRule <- many1 letter
  return $ Branch firstVar operator (read comparator) branchRule

jumpParser :: Parser Rule
jumpParser = do
  jumpRule <- many1 letter
  return $ Jump jumpRule

ruleParser :: Parser Rule
ruleParser = branchParser <|> jumpParser

-- px{a<2006:qkq,m>2090:A,rfg}
workflowParser :: Parser Workflow
workflowParser = do
  workflowName <- many1 letter
  char '{'
  rules <- ruleParser `sepBy` char ','
  char '}'
  return (workflowName, rules)

ratingParser :: Parser Rating
ratingParser = do
  string "{x="
  x <- many1 digit
  string ",m="
  m <- many1 digit
  string ",a="
  a <- many1 digit
  string ",s="
  s <- many1 digit
  char '}'
  return (read x, read m, read a, read s)

inputParser :: Parser ([Workflow], [Rating])
inputParser = do
  workflows <- workflowParser `sepBy` endOfLine
  many1 endOfLine
  ratings <- ratingParser `sepBy` endOfLine
  -- ratings <- (many1 $ notChar '\n') `sepBy` endOfLine
  return (workflows, ratings)

------------ TYPES ------------
data Rule = Branch Char Char Int String | Jump String deriving (Eq, Ord, Show)
type Workflow = (String, [Rule])
type Rating = (Int, Int, Int, Int)
type Input = ([Workflow], [Rating])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
testRule :: Rating -> Char -> Char -> Int -> Bool
testRule (x, m, a, s) var operator comparison = let
   value = case var of
     'x' -> x
     'm' -> m
     'a' -> a
     's' -> s
     c -> error ("why is the variable " ++ [c])
   in case operator of
     '>' -> value > comparison
     '<' -> value < comparison
     c -> error ("why is the operator " ++ [c])

applyWorkflow :: Rating -> [Rule] -> String
applyWorkflow rating [] = error "how did I get to the end of the workflow"
applyWorkflow _ ((Jump dest):_) = dest
applyWorkflow rating ((Branch var operator comparison dest):xs) = case testRule rating var operator comparison of
   True -> dest
   False -> applyWorkflow rating xs

applyWorkflows0 :: Rating -> Map String [Rule] -> String -> Bool
applyWorkflows0 rating wfMap thisWorkflowName = let
  rules = Map.findWithDefault (error ("bad workflow name: " ++ thisWorkflowName)) thisWorkflowName wfMap
  in case applyWorkflow rating rules of
    "A" -> True
    "R" -> False
    nextWFName -> applyWorkflows0 rating wfMap nextWFName

applyWorkflows :: Map String [Rule] -> Rating -> Bool
applyWorkflows wfMap rating = applyWorkflows0 rating wfMap "in"

ratingSum :: Rating -> Int
ratingSum (x, m, a, s) = x + m + a + s

partA :: Input -> OutputA
partA (workflows, ratings)= let
  wfMap = Map.fromList workflows
  accepted = filter (applyWorkflows wfMap) ratings
  in sum $ map ratingSum accepted

------------ PART B ------------

data RatingRange = RatingRange {
    x :: (Int, Int)
  , m :: (Int, Int)
  , a :: (Int, Int)
  , s :: (Int, Int)
} deriving Show

-- I shouldn't have to do this.
setX, setM, setA, setS :: RatingRange -> (Int, Int) -> RatingRange
setX rec n = rec { x = n }
setM rec n = rec { m = n }
setA rec n = rec { a = n }
setS rec n = rec { s = n }

splitRatingRange :: RatingRange -> Char -> Char -> Int -> (Maybe RatingRange, Maybe RatingRange)
splitRatingRange range var operator comparator = let
  (getter, setter) = case var of
    'x' -> (x, setX)
    'm' -> (m, setM)
    'a' -> (a, setA)
    's' -> (s, setS)
    c   -> error ("why is the variable " ++ [c])
  (oldMin, oldMax) = getter range
  (newMinTrue, newMaxTrue, newMinFalse, newMaxFalse) = case operator of
  -- say we had 1,4000 and we got x > 2000
  -- then we should have 2001,4000,1,2000
    '>' -> (max oldMin (comparator + 1), oldMax, oldMin, min oldMax comparator)
    -- say we got x < 2000 then 1,1999,2000,4000
    '<' -> (oldMin, min oldMax (comparator - 1), max oldMin comparator, oldMax)
    c -> error ("why is the variable " ++ [c])
  trueRange = if newMinTrue <= newMaxTrue then Just (setter range (newMinTrue, newMaxTrue)) else Nothing
  falseRange = if newMinFalse <= newMaxFalse then Just (setter range (newMinFalse, newMaxFalse)) else Nothing
  in (trueRange, falseRange)

recurseWorkflows0 :: RatingRange -> Map String [Rule] -> String -> [RatingRange]
recurseWorkflows0 range wfMap thisWorkflowName = let
  rules = Map.findWithDefault (error ("bad workflow name in recurseWorkflows0: " ++ thisWorkflowName)) thisWorkflowName wfMap
  in case thisWorkflowName of
    "A" -> [range]
    "R" -> []
    _ -> recurseRules range wfMap rules

recurseRules :: RatingRange -> Map String [Rule] ->[Rule] -> [RatingRange]
recurseRules range wfMap [] = error "why are my rules empty"
recurseRules range wfMap (r:rs) = let
  Branch var operator comparator dest = r -- only if it's a branch
  (trueRange, falseRange) = splitRatingRange range var operator comparator
  fromTrueSide = case trueRange of
    Just range0 -> recurseWorkflows0 range0 wfMap dest
    Nothing -> []
  fromFalseSide = case falseRange of
    Just range0 -> recurseRules range0 wfMap rs
    Nothing -> []
  fromBranch = fromTrueSide ++ fromFalseSide
  in case r of
    Jump wf -> recurseWorkflows0 range wfMap wf
    Branch _ _ _ _ -> fromBranch

rangeTotal :: RatingRange -> Int
rangeTotal (RatingRange x m a s) = let
  count (q1, q2) = 1 + q2 - q1
  in product $ map count [x,m,a,s]

-- partB :: Input -> OutputB
partB (workflows, ratings) = let
  wfMap = Map.fromList workflows
  ranges = recurseWorkflows0 (RatingRange (1, 4000) (1, 4000) (1, 4000) (1, 4000)) wfMap "in"
  in sum $ map rangeTotal ranges