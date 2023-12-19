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
partB :: Input -> OutputB
partB = error "Not implemented yet!"
