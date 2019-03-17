module Main where

import Data.List (maximumBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

data Nanobot = Nanobot { botCoords :: [Int], botR :: Int }
                 deriving (Eq, Show)

absDistance a b = abs (a - b)

distance :: Coords -> Nanobot -> Int
distance coords1 (Nanobot coords2 _) = sum $ zipWith absDistance coords1 coords2

strongestBot :: [Nanobot] -> Nanobot
strongestBot ls = maximumBy (\b1 b2 -> compare (botR b1) (botR b2)) ls

parseNanobot :: String -> Nanobot
parseNanobot str = let
  afterLess = drop 5 str
  xStr = takeWhile (/= ',') afterLess
  afterX = drop 1 $ dropWhile (/= ',') afterLess
  yStr = takeWhile (/= ',') afterX
  afterY = drop 1 $ dropWhile (/= ',') afterX
  zStr = takeWhile (/= '>') afterY
  rStr = drop 5 $ dropWhile (/= '>') afterY
  [xInt, yInt, zInt, r] = map (\s -> (read s :: Int)) [xStr, yStr, zStr, rStr]
  in Nanobot [xInt, yInt, zInt] r

parseFile :: String -> IO [Nanobot]
parseFile f = let
  text = readFile f
  parseText s = map parseNanobot $ lines s
  in fmap parseText text
  
nanobotsInRange :: [Nanobot] -> Nanobot -> Int
nanobotsInRange ls (Nanobot coords rC) = let
  filterFunc b2 = (distance coords b2) <= rC
  in length $ filter filterFunc ls

nanobotsInRangeOfSquare :: [Nanobot] -> Coords -> [Int]
nanobotsInRangeOfSquare ls coords = let
  filterFunc (_, b2) = (distance coords b2) <= botR b2
  pairs = zip [0..] ls
  in map fst $ filter filterFunc pairs

-- for every bot we are going to mark all the squares in its range
type SquareMap = Map Coords (Set Int)

resizeBot :: (Int -> Int) -> Nanobot -> Nanobot
resizeBot f (Nanobot coords0 r0) =
  Nanobot (map f coords0) (1 + (f r0))

resizeRange :: (Int -> Int) -> RangeND -> RangeND
resizeRange f (RangeND mins maxes) = RangeND (map f mins) (map f maxes)

resizeCoords :: (Int -> Int) -> Coords -> Coords
-- just apply the function to each element, so... map?
resizeCoords = map

addBotToMap :: Int -> RangeND -> Int -> SquareMap -> Nanobot -> SquareMap
addBotToMap divisor shrunkenRange botID sMap bot = let
  shrunkenBot = resizeBot (`div` divisor) bot
  Nanobot coords1 r1 = shrunkenBot
  squares = squaresInRadiusAndRange coords1 r1 shrunkenRange
  trace1 foo = foo --trace (show (botID, bot) ++ " after shrinking can see " ++ show squares) foo
  updateSquare sMapI square = Map.insertWith Set.union square
                               (Set.singleton botID) sMapI
  in trace1 $ foldl updateSquare sMap squares

shrunkenMapFromBots :: RangeND -> Int -> [Nanobot] -> SquareMap
shrunkenMapFromBots range divisor bots = let
  range1 = trace ("divisor: " ++ show divisor) $ resizeRange (`div` divisor) range
  botsWithIDs = zip [0..] bots
  addPairToMap sMap (botID, bot) = addBotToMap divisor range1 botID sMap bot
  in foldl addPairToMap Map.empty botsWithIDs

squareMapToRanges :: RangeND -> Int -> SquareMap -> [CandidateRange]
squareMapToRanges range divisor sMap = let
  modPair (coords, botSet) = CandidateRange (Set.size botSet)
                               (unshrinkSubrange range divisor coords)
  in reverse $ sort $ map modPair $ Map.toList sMap
  
listIndices :: [a] -> [Int] -> [a]
listIndices ls indices = let
  listIndices0 :: [b] -> [Int] -> Int -> [b]
  listIndices0 [] _ _ = []
  listIndices0 _ [] _ = []
  listIndices0 (x:xs) (i:is) curIndex
    | i == curIndex = x:(listIndices0 xs is (curIndex + 1))
    | otherwise = listIndices0 xs (i:is) (curIndex + 1)
  in listIndices0 ls indices 0

unshrinkSubrange :: RangeND -> Int -> Coords -> RangeND
unshrinkSubrange (RangeND oldMins oldMaxes) divisor shrunkenCoords = let
  unshrinkMin oldMin shrunkenCoord = max (shrunkenCoord * divisor) oldMin
  unshrinkMax oldMax shrunkenCoord = min ((shrunkenCoord + 1) * divisor) oldMax
  newMins = zipWith unshrinkMin oldMins shrunkenCoords
  newMaxes = zipWith unshrinkMax oldMaxes shrunkenCoords
  in RangeND newMins newMaxes
  
manhattanDistance :: Coords -> Int
manhattanDistance coords = sum $ map abs coords

data CandidateCoords = CandidateCoords Int Coords deriving (Eq, Show)
data CandidateRange = CandidateRange Int RangeND deriving (Eq, Show)

instance Ord CandidateCoords where
  compare (CandidateCoords b1 c1) (CandidateCoords b2 c2)
    | b1 /= b2 = compare b1 b2
    | otherwise = compare (manhattanDistance c2) (manhattanDistance c1)

instance Ord CandidateRange where
  compare (CandidateRange b1 _) (CandidateRange b2 _) = compare b1 b2
--    | b1 /= c1 = compare b1 b2
--    | otherwise = compare (manhattanDistance c2) (manhattanDistance c1)

coordsBeatsRange :: CandidateCoords -> CandidateRange -> Bool
coordsBeatsRange (CandidateCoords b1 _) (CandidateRange b2 _) = b1 > b2

recurseAcross :: Int -> [Nanobot] -> Int -> Int -> CandidateCoords -> [CandidateRange] -> CandidateCoords
recurseAcross depth bots granularity cutoff bestSoFar [] = bestSoFar
recurseAcross depth bots granularity cutoff bestSoFar sortedCandidates
  | trace ("recurseAcross " ++ show (depth, cutoff, nextBotCount, bestSoFar) ++ " candidates left: " ++ show (length sortedCandidates)) False = undefined
  | newCutoff > nextBotCount = bestSoFar
  | newCutoff > nextBotCount = bestSoFar
  | trace ("result of nextCandidate is " ++ show nextResult) False = undefined
  | otherwise = recurseAcross depth bots granularity newCutoff newBest (tail sortedCandidates)
  where nextCandidate = head sortedCandidates
        CandidateRange nextBotCount _ = nextCandidate
        CandidateCoords bestBotCount _ = bestSoFar
        newCutoff = max cutoff bestBotCount    
        nextResult = recurseDown (depth + 1) bots granularity newCutoff nextCandidate
        newBest = max bestSoFar nextResult

rangeToCoords :: RangeND -> Coords
rangeToCoords (RangeND mins maxes)
  | mins /= maxes = error "cannot convert a wide range"
  | otherwise = mins

recurseDown :: Int -> [Nanobot] -> Int -> Int -> CandidateRange -> CandidateCoords
recurseDown depth bots granularity cutoff cRange
  | trace("recurseDown: " ++ show (depth, rangeSize, cutoff, cRange)) False = undefined
  | rangeSize == 0 || divisor == 0 = bruteForce bots range
  | trace ("sMap: " ++ showMap sMap) False = undefined
  | trace ("bMap: " ++ showMap bMap) False = undefined
  | otherwise = recurseAcross depth bots granularity cutoff nullCandidate candidates
  where
    CandidateRange _ range = cRange
    rangeSize = maxRangeSize range
    divisor = rangeSize `div` granularity
    nullCandidate = CandidateCoords (-1) [1979, 1979, 1979]
    sMap = shrunkenMapFromBots range divisor bots
    bMap = betterSquareMap sMap
    candidates = squareMapToRanges range divisor bMap

bruteForce :: [Nanobot] -> RangeND -> CandidateCoords
bruteForce bots range = let
  squares = squaresInRange range
  makeCandidate square = CandidateCoords (inRangeOfBots bots square) square
  candidates = map makeCandidate squares
  in maximum candidates

maxRangeSize :: RangeND -> Int
maxRangeSize (RangeND mins maxes) = maximum $ zipWith (-) maxes mins

showMap :: SquareMap -> String
showMap m = let
  pairs = Map.toList m
  simplerPairs = map (\(x, y) -> (x, Set.size y)) pairs
  in show simplerPairs

pairWithMaxSize :: SquareMap -> (Coords, Set Int)
pairWithMaxSize sMap = let
  compareOnSndSize x y = compare (Set.size (snd x)) (Set.size (snd y))
  in maximumBy compareOnSndSize $ Map.toList sMap

confusingCombinationsByPosition :: [[a]] -> [[a]]
-- I want to take a list of possible first elements, a list of possible second
-- elements, etc, and then make all the possible combinations given those
-- constraints.
confusingCombinationsByPosition [] = [[]]
confusingCombinationsByPosition (lx:lxs) = let
  tails = confusingCombinationsByPosition lxs
  in [(h:t) | h <- lx, t <- tails]

squaresInRadiusAndRange :: Coords -> Int -> RangeND -> [Coords]
squaresInRadiusAndRange coords r (RangeND mins maxes) = let
  makeMinOffset c min = max (-r) (min - c)
  minOffsets = zipWith makeMinOffset coords mins
  makeMaxOffset c max = min r (max - c)
  maxOffsets = zipWith makeMaxOffset coords maxes
  makeMinMaxList vMin vMax = [vMin..vMax]
  deltaRanges = zipWith makeMinMaxList minOffsets maxOffsets
  allPossibleDeltas = confusingCombinationsByPosition deltaRanges
  deltaRangesInRadius = filter (\c -> manhattanDistance c <= r)
                          allPossibleDeltas
  deltaRangeToCoords dRange = zipWith (+) coords dRange
  in map deltaRangeToCoords deltaRangesInRadius

squaresToCheckForCube :: Coords -> [Coords]
squaresToCheckForCube coords = let
  higherCoords = map (+1) coords
  in squaresInRange (RangeND coords higherCoords)

betterSquareMap :: SquareMap -> SquareMap
betterSquareMap old = let
  -- for each map key in the old map
  -- the new map has the sum of squaresToCheckForCube
  getBots coords = Map.findWithDefault Set.empty coords old
  newValue coords = foldl Set.union Set.empty $ map getBots $ squaresToCheckForCube coords
  makePair coords = (coords, newValue coords)
  pairs = map makePair $ Map.keys old
  in Map.fromList pairs

squaresInRange :: RangeND -> [Coords]
squaresInRange (RangeND mins maxes) = let
  makeList rMin rMax = [rMin..rMax]
  listOfRanges = zipWith makeList mins maxes
  -- now I have a LoL [[xMin...xMax], [yMin..yMax]... etc]
  in confusingCombinationsByPosition listOfRanges

inRangeOfBot :: Coords -> Nanobot -> Bool
inRangeOfBot coords bot = (distance coords bot) <= botR bot

inRangeOfBots :: [Nanobot] -> Coords -> Int
inRangeOfBots bots coords = length $ filter (inRangeOfBot coords) bots

solvePart1 :: [Nanobot] -> Int
solvePart1 ls = let
  strongest = strongestBot ls
  in nanobotsInRange ls strongest

type Coords = [Int]
data RangeND = RangeND [Int] [Int] deriving (Eq, Show)

botRange :: [Nanobot] -> RangeND
botRange bots = let
  botRange0 [] oldRange = oldRange
  botRange0 ((Nanobot coords _):xs) (RangeND oldMins oldMaxes) = let
    newMins = zipWith min coords oldMins
    newMaxes = zipWith max coords oldMaxes
    in botRange0 xs (RangeND newMins newMaxes)
  (Nanobot coords0 _) = head bots
  in botRange0 (tail bots) (RangeND coords0 coords0)

moreThanOneElem :: [a] -> Bool
moreThanOneElem ls = null $ tail ls

solvePart2 :: [Nanobot] -> Int -> (Int, Coords, Int)
solvePart2 bots cutoff = let
  range = botRange bots
  granularity = 2
  solution = recurseDown 1 bots granularity cutoff (CandidateRange (length bots) range)
  CandidateCoords count coords = solution
  in (manhattanDistance coords, coords, count)

main :: IO ()
main = do
  testBots <- parseFile "input/Advent2018d23test.txt"
  putStrLn $ show $ solvePart1 testBots
  bots <- parseFile "input/Advent2018d23.txt"
  putStrLn $ show $ solvePart1 bots
  testBots2 <- parseFile "input/Advent2018d23test2.txt"
  putStrLn $ show $ solvePart2 testBots2 0
  putStrLn $ show $ solvePart2 bots 900
