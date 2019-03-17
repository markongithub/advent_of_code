module Main where

import Data.List (maximumBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)

data Nanobot = Nanobot { botX :: Int, botY :: Int, botZ :: Int, botR :: Int }
                 deriving (Eq, Show)

distance :: (Int, Int, Int) -> Nanobot -> Int
distance (x1, y1, z1) (Nanobot x2 y2 z2 _) =
  (abs (x1 - x2)) + (abs (y1 - y2)) + (abs (z1 - z2))

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
  in Nanobot xInt yInt zInt r

parseFile :: String -> IO [Nanobot]
parseFile f = let
  text = readFile f
  parseText s = map parseNanobot $ lines s
  in fmap parseText text
  
nanobotsInRange :: [Nanobot] -> Nanobot -> Int
nanobotsInRange ls (Nanobot xC yC zC rC) = let
  filterFunc b2 = (distance (xC, yC, zC) b2) <= rC
  in length $ filter filterFunc ls

nanobotsInRangeOfSquare :: [Nanobot] -> Coords -> [Int]
nanobotsInRangeOfSquare ls (x, y, z) = let
  filterFunc (_, b2) = (distance (x, y, z) b2) <= botR b2
  pairs = zip [0..] ls
  in map fst $ filter filterFunc pairs

-- for every bot we are going to mark all the squares in its range
-- Map (Int, Int, Int) Int

type SquareMap = Map Coords (Set Int)

resizeBot :: (Int -> Int) -> Nanobot -> Nanobot
resizeBot f (Nanobot x0 y0 z0 r0) =
  Nanobot (f x0) (f y0) (f z0) (1 + (f r0))

resizeRange :: (Int -> Int) -> Range3D -> Range3D
resizeRange f (a0, b0, c0, d0, e0, f0) = let
  [a1, b1, c1, d1, e1, f1] = map f [a0, b0, c0, d0, e0, f0]
  in (a1, b1, c1, d1, e1, f1)

resizeCoords :: (Int -> Int) -> Coords -> Coords
resizeCoords f (x, y, z) = let
  [x2, y2, z2] = map f [x, y, z]
  in (x2, y2, z2)

addBotToMap :: Int -> Range3D -> Int -> SquareMap -> Nanobot -> SquareMap
addBotToMap divisor shrunkenRange botID map bot = let
  shrunkenBot = resizeBot (`div` divisor) bot
  Nanobot x1 y1 z1 r1 = shrunkenBot
  squares = squaresInRadiusAndRange (x1, y1, z1) r1 shrunkenRange
  trace1 foo = foo --trace (show (botID, bot) ++ " after shrinking can see " ++ show squares) foo
  updateSquare mapI square = Map.insertWith Set.union square
                               (Set.singleton botID) mapI
  in trace1 $ foldl updateSquare map squares

shrunkenMapFromBots :: Range3D -> Int -> [Nanobot] -> SquareMap
shrunkenMapFromBots range divisor bots = let
  range1 = trace ("divisor: " ++ show divisor) $ resizeRange (`div` divisor) range
  botsWithIDs = zip [0..] bots
  addPairToMap map (botID, bot) = addBotToMap divisor range1 botID map bot
  in foldl addPairToMap Map.empty botsWithIDs

squareMapToRanges :: Range3D -> Int -> SquareMap -> [CandidateRange]
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

unshrinkSubrange :: Range3D -> Int -> Coords -> Range3D
unshrinkSubrange oldRange divisor (x, y, z) = let
  (oldXMin, oldXMax, oldYMin, oldYMax, oldZMin, oldZMax) = oldRange
  newXMin = max (x * divisor) oldXMin
  newXMax = min ((x + 1) * divisor) oldXMax
  newYMin = max (y * divisor) oldYMin
  newYMax = min ((y + 1) * divisor) oldYMax
  newZMin = max (z * divisor) oldZMin
  newZMax = min ((z + 1) * divisor) oldZMax
  in (newXMin, newXMax, newYMin, newYMax, newZMin, newZMax)
  
manhattanDistance :: Coords -> Int
manhattanDistance (x, y, z) = abs x + abs y + abs z

data CandidateCoords = CandidateCoords Int Coords deriving (Eq, Show)
data CandidateRange = CandidateRange Int Range3D deriving (Eq, Show)

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
        CandidateRange nextBotCount nextRange = nextCandidate
        nextRangeSize = maxRangeSize nextRange
        CandidateCoords bestBotCount _ = bestSoFar
        newCutoff = max cutoff bestBotCount    
        nextResult = recurseDown (depth + 1) bots granularity newCutoff nextCandidate
        newBest = max bestSoFar nextResult

rangeToCoords :: Range3D -> Coords
rangeToCoords (x1, x2, y1, y2, z1, z2)
  | (x1 /= x2) || (y1 /= y2) || (z1 /= z2) = error "cannot convert a wide range"
  | otherwise = (x1, y1, z1)


recurseDown :: Int -> [Nanobot] -> Int -> Int -> CandidateRange -> CandidateCoords
recurseDown depth bots granularity cutoff cRange
  | trace("recurseDown: " ++ show (depth, rangeSize, cutoff, cRange)) False = undefined
  | rangeSize == 0 || divisor == 0 = bruteForce bots range
  | trace ("sMap: " ++ showMap sMap) False = undefined
  | trace ("bMap: " ++ showMap bMap) False = undefined
  | otherwise = recurseAcross depth bots granularity cutoff nullCandidate candidates
  where
    CandidateRange botCount range = cRange
    rangeSize = maxRangeSize range
    divisor = rangeSize `div` granularity
    nullCandidate = CandidateCoords (-1) (1979, 1979, 1979)
    sMap = shrunkenMapFromBots range divisor bots
    bMap = betterSquareMap sMap
    candidates = squareMapToRanges range divisor bMap

bruteForce :: [Nanobot] -> Range3D -> CandidateCoords
bruteForce bots range = let
  squares = squaresInRange range
  makeCandidate square = CandidateCoords (inRangeOfBots bots square) square
  candidates = map makeCandidate squares
  in maximum candidates

maxRangeSize :: Range3D -> Int
maxRangeSize (a, b, c, d, e, f) = maximum [b-a, d-c, f-e]

showMap :: SquareMap -> String
showMap m = let
  pairs = Map.toList m
  simplerPairs = map (\(x, y) -> (x, Set.size y)) pairs
  in show simplerPairs

pairWithMaxSize :: SquareMap -> (Coords, Set Int)
pairWithMaxSize map = let
  compareOnSndSize x y = compare (Set.size (snd x)) (Set.size (snd y))
  in maximumBy compareOnSndSize $ Map.toList map

squaresInRadiusAndRange :: Coords -> Int -> Range3D -> [Coords]
squaresInRadiusAndRange (x0, y0, z0) r (xMin, xMax, yMin, yMax, zMin, zMax) = let
  xMinOffset :: Int
  xMinOffset = max (-r) (xMin - x0)
  xMaxOffset :: Int
  xMaxOffset = min r (xMax - x0)
  xdRange :: [Int]
  xdRange = [xMinOffset..xMaxOffset]
  yMinOffset = max (-r) (yMin - y0)
  yMaxOffset = min r (yMax - y0)
  zMinOffset = max (-r) (zMin - z0)
  zMaxOffset = min r (zMax - z0)
  in [(x0 + xd, y0 + yd, z0 + zd) | xd <- xdRange,
                                    yd <- [yMinOffset..yMaxOffset],
                                    zd <- [zMinOffset..zMaxOffset],
                                    (abs xd + abs yd + abs zd) <= r ]

squaresToCheckForCube :: Coords -> [Coords]
squaresToCheckForCube (x, y, z) = squaresInRange (x, x+1, y, y+1, z, z+1)

betterSquareMap :: SquareMap -> SquareMap
betterSquareMap old = let
  -- for each map key in the old map
  -- the new map has the sum of squaresToCheckForCube
  getBots coords = Map.findWithDefault Set.empty coords old
  newValue coords = foldl Set.union Set.empty $ map getBots $ squaresToCheckForCube coords
  makePair coords = (coords, newValue coords)
  pairs = map makePair $ Map.keys old
  in Map.fromList pairs


squaresInRange :: Range3D -> [(Int, Int, Int)]
squaresInRange (xMin, xMax, yMin, yMax, zMin, zMax) =
  [(xR, yR, zR) | xR <- [xMin..xMax],
                  yR <- [yMin..yMax],
                  zR <- [zMin..zMax]]

inRangeOfBot :: (Int, Int, Int) -> Nanobot -> Bool
inRangeOfBot (x0, y0, z0) bot = let
  (Nanobot x1 y1 z1 r1) = bot
  in (distance (x0, y0, z0) bot) <= r1

inRangeOfBots :: [Nanobot] -> (Int, Int, Int) -> Int
inRangeOfBots bots coords = length $ filter (inRangeOfBot coords) bots

solvePart1 :: [Nanobot] -> Int
solvePart1 ls = let
  strongest = strongestBot ls
  in nanobotsInRange ls strongest

type Range3D = (Int, Int, Int, Int, Int, Int)
type Coords = (Int, Int, Int)

data RangeND = RangeND [Int] [Int]

botRange :: [Nanobot] -> Range3D
botRange bots = let
  botRange0 [] xMin xMax yMin yMax zMin zMax =
    (xMin, xMax, yMin, yMax, zMin, zMax)
  botRange0 ((Nanobot x0 y0 z0 _):xs) xMin xMax yMin yMax zMin zMax = let
    newXMin = min xMin x0
    newXMax = max xMax x0
    newYMin = min yMin y0
    newYMax = max yMax y0
    newZMin = min zMin z0
    newZMax = max zMax z0
    in botRange0 xs newXMin newXMax newYMin newYMax newZMin newZMax
  (Nanobot x0 y0 z0 _) = head bots
  in botRange0 (tail bots) x0 x0 y0 y0 z0 z0

moreThanOneElem :: [a] -> Bool
moreThanOneElem ls = null $ tail ls

solvePart2 :: [Nanobot] -> (Int, Coords, Int)
solvePart2 bots = let
  range = botRange bots
  granularity = 2
  solution = recurseDown 1 bots granularity 900 (CandidateRange (length bots) range)
  CandidateCoords count coords = solution
  in (manhattanDistance coords, coords, count)

-- 98125627 is wrong but
-- 99843343 (914) is wrong
-- BUG: recurseDown: (900,CandidateRange 966 (11384343,22768686,56921715,68306058,22768687,34153029))
-- That is wrong, it should have at least 978.
main :: IO ()
main = do
--  testBots <- parseFile "input/Advent2018d23test.txt"
--  putStrLn $ show $ solvePart1 testBots
  bots <- parseFile "input/Advent2018d23.txt"
--  putStrLn $ show $ solvePart1 bots
  --testBots2 <- parseFile "input/Advent2018d23test2.txt"
  -- putStrLn $ show $ solvePart2 testBots2
  putStrLn $ show $ solvePart2 bots
