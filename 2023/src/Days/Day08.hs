module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Debug.Trace (traceShow)
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
nodeParser :: Parser Node
nodeParser = do
  source <- many1 letter
  string " = ("
  leftDest <- many1 letter
  string ", "
  rightDest <- many1 letter
  char ')'
  return (source, leftDest, rightDest)

inputParser :: Parser Input
inputParser = do
  directions <- many1 letter
  many1 endOfLine
  nodes <- nodeParser `sepBy` endOfLine
  return (directions, nodes)

------------ TYPES ------------
type Node = (String, String, String)
type Input = (String, [Node])
type Graph = Map String (String, String)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
makeGraph :: [Node] -> Graph
makeGraph nodes = let
  toPair (source, left, right) = (source, (left, right))
  in Map.fromList $ map toPair nodes

followDirection :: Graph -> String -> Char -> String
followDirection g source dir = let
  (left, right) = case Map.lookup source g of
    Just result -> result
    Nothing -> error ("invalid source node: " ++ source)
  in case dir of
    'L' -> left
    'R' -> right
    _ -> error ("invalid direction: " ++ show dir)

routeToZ :: Graph -> String -> String -> [String]
routeToZ g "ZZZ" _ = []
routeToZ g source [] = error ("ran out of directions at " ++ source)
routeToZ g source (x:xs) = let
  nextSource = followDirection g source x
  in (source:routeToZ g nextSource xs)

-- stepsToZ :: Graph -> String -> String -> Int
-- stepsToZ g s dirs = length $ routeToZ g s dirs

stepsToZ :: Graph -> String -> String -> Int -> Int
stepsToZ g source directions count = snd $ stepsToGoal3 g (== "ZZZ") source directions count

funcIndex :: (a -> Bool) -> [a] -> Maybe (a, Int)
funcIndex f ls = funcIndex0 f ls 0

funcIndex0 :: (a -> Bool) -> [a] -> Int -> Maybe (a, Int)
funcIndex0 f [] _ = Nothing
funcIndex0 f (x:xs) count = if f x then Just (x, count) else funcIndex0 f xs (count + 1)

stepsToGoal :: Graph -> (String -> Bool) -> String -> String -> Int -> (String, Int)
stepsToGoal g f source directions count = let
  addStep :: [String] -> Char -> [String]
  addStep ls direction = (followDirection g (head ls) direction ):ls
  allSteps = foldl addStep [source] directions
  newCount = count + length directions
  debugMessage = "after " ++ show newCount ++ " steps we are at " ++ show (head allSteps)
  recurse = traceShow debugMessage $ stepsToGoal g f (head allSteps) directions (count + length directions)
  in case funcIndex f (init allSteps) of
    Just (winner, n) -> undefined -- (winner, count + (length directions) - n)
    Nothing -> recurse

partA :: Input -> OutputA
partA (directions, nodes) = let
  graph = makeGraph nodes
  in stepsToZ graph "AAA" (concat $ repeat directions) 0

------------ PART B ------------

followDirectionParallel :: Graph -> [String] -> Char -> [String]
followDirectionParallel g sources dir = let
  advance source = followDirection g source dir
  in map advance sources

stepsToZ2 :: Graph -> (String -> Bool) -> [String] -> String -> Int -> Int -> Int
stepsToZ2 _ _ _ [] _ _ = error "Directions should never be empty"
stepsToZ2 g termFunc sources (dir:ds) count debugPeriod = let
  nextSources2 = followDirectionParallel g sources dir
  debugMessage = "after " ++ show count ++ " steps we are at " ++ show sources
  nextSources = if debugPeriod == 0 then traceShow debugMessage nextSources2 else nextSources2
  nextDebug = undefined
  recurse = stepsToZ2 g termFunc nextSources ds (count + 1)
  in case (all termFunc sources) of
    True -> count
    False -> undefined

stepsToGoal2 :: Graph -> (String -> Bool) -> String -> String -> Int -> (String, Int)
stepsToGoal2 g f source directions count = let
  nextSource = followDirection g source (head directions)
  recurse = stepsToGoal2 g f nextSource (tail directions) (count + 1)
  in if f source then (source, count) else recurse

stepsToGoal3 :: Graph -> (String -> Bool) -> String -> String -> Int -> (String, Int)
stepsToGoal3 g f source directions count = let
  nextSource = followDirection g source (head directions)
  recurse = stepsToGoal2 g f nextSource (tail directions) (count + 1)
  in recurse

foldButGenerateList :: (a -> b -> a) -> a -> [b] -> [a]
foldButGenerateList func start [] = [start]
foldButGenerateList func start (x:xs) = let
  recurse = foldButGenerateList func (func start x) xs
  in (start:recurse)

lookForZLoop :: Graph -> String -> String -> Int -> [(String, Int)]
lookForZLoop graph source directions count = let
  infiniteSteps :: [String]
  infiniteSteps = foldButGenerateList (followDirection graph) source (concat $ repeat directions)
  numberedSteps :: [(String, Int)]
  numberedSteps = zip infiniteSteps [0..]
  filterFunc :: (String, Int) -> Bool
  filterFunc (s, _) = last s == 'Z'
  matches = filter filterFunc numberedSteps
  in Data.List.take count matches

findLoopLength :: Graph -> String -> String -> Int
findLoopLength graph source directions = snd $ head $ lookForZLoop graph source directions 1

partB :: Input -> Int
partB (directions, nodes) = let
    graph = makeGraph nodes
    fst3 (x, _, _) = x
    sources = filter (\s -> last s == 'A') $ map fst3 nodes
    loopLengths = map (\s -> findLoopLength graph s directions) sources
    multiple = foldl lcm 1 loopLengths
    in multiple

