module Main where

import Data.List (maximumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShow)

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

-- for every bot we are going to mark all the squares in its range
-- Map (Int, Int, Int) Int

type SquareMap = Map (Int, Int, Int) (Set Int)

resizeBot :: (Int -> Int) -> Nanobot -> Nanobot
resizeBot f (Nanobot x0 y0 z0 r0) =
  Nanobot (f x0) (f y0) (f z0) (f r0)

resizeRange :: (Int -> Int) -> Range3D -> Range3D
resizeRange f (a0, b0, c0, d0, e0, f0) = let
  [a1, b1, c1, d1, e1, f1] = map f [a0, b0, c0, d0, e0, f0]
  in (a1, b1, c1, d1, e1, f1)

addBotToMap :: Int -> Range3D -> Int -> SquareMap -> Nanobot -> SquareMap
addBotToMap divisor shrunkenRange botID map bot = let
  Nanobot x1 y1 z1 r1 = resizeBot (`div` divisor) bot
  maxDistance = 102111240 `div` divisor
  -- maxDistance = 101990033 `div` divisor
  squares = filter (\c -> distanceZero c < maxDistance) $ squaresInRadiusAndRange (x1, y1, z1) r1 shrunkenRange maxDistance
  -- squares = squaresInRadiusAndRange (x1, y1, z1) r1 shrunkenRange maxDistance
  updateSquare mapI square = Map.insertWith Set.union square
                               (Set.singleton botID) mapI
  in foldl updateSquare map squares

shrunkenMapFromBots :: Range3D -> Int -> [Nanobot] -> SquareMap
shrunkenMapFromBots range divisor bots = let
  range1 = resizeRange (`div` divisor) range
  botsWithIDs = zip [0..] bots
  addPairToMap map (botID, bot) = addBotToMap divisor range1 botID map bot
  in foldl addPairToMap Map.empty botsWithIDs

listIndices :: [a] -> [Int] -> [a]
listIndices ls indices = let
  listIndices0 :: [b] -> [Int] -> Int -> [b]
  listIndices0 [] _ _ = []
  listIndices0 _ [] _ = []
  listIndices0 (x:xs) (i:is) curIndex
    | i == curIndex = x:(listIndices0 xs is (curIndex + 1))
    | otherwise = listIndices0 xs (i:is) (curIndex + 1)
  in listIndices0 ls indices 0

narrowProblem :: Range3D -> [Nanobot] -> Int -> (Range3D, [Nanobot])
narrowProblem range bots divisor = let
  map = shrunkenMapFromBots range divisor bots
  ((x, y, z), set) = pairWithMaxSize map
  (oldXMin, oldXMax, oldYMin, oldYMax, oldZMin, oldZMax) = range
  newXMin = max (x * divisor) oldXMin
  newXMax = min ((x + 1) * divisor) oldXMax
  newYMin = max (y * divisor) oldYMin
  newYMax = min ((y + 1) * divisor) oldYMax
  newZMin = max (z * divisor) oldZMin
  newZMax = min ((z + 1) * divisor) oldZMax
  newRange = (newXMin, newXMax, newYMin, newYMax, newZMin, newZMax)
  newBots = bots -- listIndices bots (Set.toList set)
  in traceShow (Set.size set) (newRange, newBots)

maxRangeSize :: Range3D -> Int
maxRangeSize (a, b, c, d, e, f) = maximum [b-a, d-c, f-e]

triangulate2 :: [Nanobot] -> Range3D -> Int -> Coords
triangulate2 bots range granularity
  | traceShow (range, length bots) False = undefined
  | allDone = (xMin, yMin, zMin)
  | otherwise = recurse
  where
    divisions = max 1 $ (maxRangeSize range) `div` granularity
    (nextRange, nextBots) = narrowProblem range bots divisions
    (xMin, xMax, yMin, yMax, zMin, zMax) = nextRange
    recurse = triangulate2 nextBots nextRange granularity
    allDone = ((xMax - xMin) <= 1) && ((yMax - yMin) <= 1) && ((zMax - zMin) <= 1)

pairWithMaxSize :: SquareMap -> (Coords, Set Int)
pairWithMaxSize map = let
  compareOnSndSize x y = compare (Set.size (snd x)) (Set.size (snd y))
  in if Map.null map then error "the square map is empty" else maximumBy compareOnSndSize $ Map.toList map

squaresInRadiusAndRange :: Coords -> Int -> Range3D -> Int -> [Coords]
squaresInRadiusAndRange (x0, y0, z0) r (xMin, xMax, yMin, yMax, zMin, zMax) maxDistance = let
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
                                    (abs xd + abs yd + abs zd) <= r]
  --                                  ((abs (y0 + yd)) + (abs (y0 + yd)) + (abs (z0 + zd))) <= maxDistance ]

distanceZero :: Coords -> Int
distanceZero (x, y, z) = abs x + abs y + abs z

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

divideRange minR maxD 0 = []
divideRange minR maxD divisions = let
  divisor = divisions + 1
  increment = max 1 ((maxD - minR) `div` divisor)
  nextStep = minR + increment
  recurse = nextStep : (divideRange nextStep maxD (divisions - 1))
  in if (divisions > (maxD - minR))
       then [minR..maxD]
       else if (minR == maxD || nextStep == maxD) then [maxD] else recurse

figureError :: Int -> Int -> Int -> Int
figureError min max divisions
  | divisions > (max + 1 - min) = 0
  | rem == 0 = quot
  | otherwise = quot + 1
  where (quot, rem) = (max - min) `divMod` (divisions + 1)

divideRange3D :: Range3D -> Int -> (Coords, [Coords])
divideRange3D (xMin, xMax, yMin, yMax, zMin, zMax) divisions = let
  xs = divideRange xMin xMax divisions
  xErr = figureError xMin xMax divisions
  ys = divideRange yMin yMax divisions
  yErr = figureError yMin yMax divisions
  zs = divideRange zMin zMax divisions
  zErr = figureError zMin zMax divisions
  coords = [(x0, y0, z0) | x0 <- xs, y0 <- ys, z0 <- zs]
  in ((xErr, yErr, zErr), coords)

moreThanOneElem :: [a] -> Bool
moreThanOneElem ls = null $ tail ls

narrowRange :: [Nanobot] -> Range3D -> Int -> Range3D
narrowRange bots range divisions = let
  ((xErr, yErr, zErr), allSquares) = divideRange3D range divisions
  allPairs = zip allSquares (map (inRangeOfBots bots) allSquares)
  maxPair = maximumBy (\b1 b2 -> compare (snd b1) (snd b2)) allPairs
  (bestX, bestY, bestZ) = fst maxPair
  in (bestX - xErr, bestX + xErr, bestY - yErr, bestY + yErr,
      bestZ - zErr, bestZ + zErr)

triangulate :: [Nanobot] -> Range3D -> Int -> Coords
triangulate bots range divisions
  | traceShow range False = undefined
  | allDone = (xMin, yMin, zMin)
  | otherwise = recurse
  where 
    nextStep = narrowRange bots range divisions
    (xMin, xMax, yMin, yMax, zMin, zMax) = nextStep
    recurse = triangulate bots nextStep divisions
    allDone = xMin == xMax && yMin == yMax && zMin == zMax

solvePart2 :: [Nanobot] -> (Coords, Int)
solvePart2 bots = let
  allSquares :: [(Int, Int, Int)]
  allSquares = squaresInRange $ botRange bots
  -- 17 works with a maxDistance
  bestSquare = triangulate2 bots (botRange bots) 17
  (c0, c1, c2) = bestSquare
  in (bestSquare, abs c0 + abs c1 + abs c2)

bigProblem :: IO (Range3D, [Nanobot])
bigProblem = do
  bots <- parseFile "input/Advent2018d23.txt"
  return (botRange bots, bots)

stateOfTheArt :: IO (Range3D, [Nanobot])
stateOfTheArt = do
  bots <- parseFile "input/Advent2018d23.txt"
  let (range2, bots2) = narrowProblem (botRange bots) bots 10000000
  return (range2, bots2)


((14094162,58871033,27147936),909)

main :: IO ()
main = do
  testBots <- parseFile "input/Advent2018d23test.txt"
  putStrLn $ show $ solvePart1 testBots
  bots <- parseFile "input/Advent2018d23.txt"
  putStrLn $ show $ solvePart1 bots
  testBots2 <- parseFile "input/Advent2018d23test2.txt"
  putStrLn $ show $ solvePart2 testBots2
  putStrLn $ show $ solvePart2 bots


