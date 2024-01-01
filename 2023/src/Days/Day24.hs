module Days.Day24 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Debug.Trace (traceShow)
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
floatParserThatActuallyWorks :: Parser Float
floatParserThatActuallyWorks = do
  digits <- many1 (satisfy $ inClass "01234566789-")
  return $ read digits

lineParser :: Parser (Position, Velocity)
lineParser = do
  xPos <- floatParserThatActuallyWorks
  char ','
  many1 space
  yPos <- floatParserThatActuallyWorks
  char ','
  many1 space
  zPos <- floatParserThatActuallyWorks
  string " @ "
  xVel <- floatParserThatActuallyWorks
  char ','
  many1 space
  yVel <- floatParserThatActuallyWorks
  char ','
  many1 space
  zVel <- floatParserThatActuallyWorks
  return ((xPos, yPos, zPos), (xVel, yVel, zVel))

inputParser :: Parser Input
inputParser = lineParser `sepBy` endOfLine

------------ TYPES ------------
type Position = (Float, Float, Float)
type Velocity = (Float, Float, Float)
type Input = [(Position, Velocity)]

type OutputA = Void

type OutputB = Void

------------ PART A ------------
{-
Hailstone A: 19, 13, 30 @ -2, 1, -2
Hailstone B: 18, 19, 22 @ -1, -1, -2
Hailstones' paths will cross inside the test area (at x=14.333, y=15.333).

19 + -2a = 18 + -1b
13 + 1a = 19 + -1b

they want x=14.333, y=15.333
if x =14.333 then a=7/3 b=11/3
if y=15.333 then a=7/3 b=11/3
19 -2a = 18 -b = 14.333
19 -2a +b = 18
1 -2a + b = 0
-2a + b + 1 = 0

13 + a = 19 - b
a + b -6 = 0

-2a + b + 1 = 0
a + b -6 = 0

-3a + 7 = 0
a =7/3

19 + -2a = 18 + -1b
x1 + v1a = x2 + v1b
vx1a + (-1 * vx2)b = x2 - x1
vy1a + (-1 * vy2)b = y2 - y1
Multiply first row by (-1 * vy1a/vx1a) and add the result to second row
const = 
add (-1 * vy1a / )

19 + -2a = 18 + -1b
13 + 1a = 19 + -1b
-2a - (-1b) = 18-19
1a - (-1b) = 19-13

[-2, 1, -1]
[ 1, 1,  6]
-2 1 -1
 0 1 3.67

 -2a + 1b = -1
 -2a + 1(3.67) = -1
 -2a = -1 - 
 rightmost value minus (sum of coefficients * values) divided by first coefficient

[3.67]
-}

type Matrix = (Int, Int, Map (Int, Int) Float)

valuesFromRow :: Matrix -> Int -> Int -> [Float]
valuesFromRow (maxX, _, grid) row minX = let
  getCol c = mapGet (c, row) grid
  in map getCol [minX..maxX]

replaceRow :: Matrix -> Int -> Int -> [Float] -> Matrix
replaceRow (maxX, maxY, grid) row minX values = let
  coords = zip [minX..maxX] (repeat row)
  pairs = zip coords values
  in (maxX, maxY, Map.union (Map.fromList pairs) grid)

reduceRows :: Matrix -> Int -> Int -> Int -> Matrix
reduceRows matrix topRow bottomRow col = let
  (maxX, maxY, grid) = matrix
  topVal = mapGet (col, topRow) grid
  bottomVal = mapGet (col, bottomRow) grid
  constant = -1.0 * (bottomVal / topVal)
  topAddend = map (* constant) $ valuesFromRow matrix topRow col
  bottomAddend = valuesFromRow matrix bottomRow col
  newRowValues = zipWith (+) topAddend bottomAddend
  in replaceRow matrix bottomRow col newRowValues

mapGet :: (Ord k, Show k) => k -> Map k a -> a
mapGet key m = case Map.lookup key m of
  Just a -> a
  Nothing -> error ("you are dumb because you thought this map would contain " ++ show key)

reduceRowByConstant :: Matrix -> Int -> Matrix
reduceRowByConstant matrix row = let
  (maxX, maxY, grid) = matrix
  startCol = maxX - row - 1
  rowValues = valuesFromRow matrix row startCol
  divisor = head rowValues
  newValues = map (/ divisor) rowValues
  in replaceRow matrix row startCol newValues

solveBottomRow :: Matrix -> Matrix
solveBottomRow matrix = reduceRowByConstant matrix 0

matrixFromRows :: [[Float]] -> Matrix
matrixFromRows rows = let
  maxY = (length rows) - 1
  maxX = (length $ head rows) - 1
  flippedRows = reverse rows
  coordsByRow :: Int -> [(Int, Int)]
  coordsByRow r = zip [0..] (repeat r)
  rowsWithIndices = zip [0..] flippedRows
  fixCoords :: (Int, [Float]) -> [((Int, Int), Float)]
  fixCoords (rowIndex, rowValues) = zip (coordsByRow rowIndex) rowValues
  pairs :: [((Int, Int), Float)]
  pairs = concat $ map fixCoords rowsWithIndices
  in (maxX, maxY, Map.fromList pairs)

backSubstitute0 :: Matrix -> Int -> [Float] -> [Float]
backSubstitute0 matrix row valuesSoFar = let
  (maxX, maxY, grid) = matrix
  -- we are in row N and we have N values
  -- we are going to plug them into cells (maxX - N, row) to (maxX - 1, row)
  --  rightmost value minus (sum of coefficients * values) divided by first coefficient
  rowValues = valuesFromRow matrix row (maxX - row - 1)
  (thisCoefficient:otherValues) = rowValues
  coefficients = Data.List.take row otherValues
  coefficientProducts = zipWith (*) coefficients valuesSoFar
  [lastCell] = drop row otherValues
  newValue = (lastCell - sum coefficientProducts) / thisCoefficient
  newValues = newValue:valuesSoFar
  -- if row == maxX then return else recurse to row - 1
  in if row == maxY then newValues else backSubstitute0 matrix (row + 1) newValues

backSubstitute :: Matrix -> [Float]
backSubstitute matrix = backSubstitute0 matrix 0 []

descendMatrix0 :: Matrix -> Int -> Matrix
descendMatrix0 matrix row = let
  (maxX, _, _) = matrix
  matrix2 = reduceRowByConstant matrix row
  matrix3 = reduceRows matrix row (row - 1) (maxX - row - 1)
  recurse = descendMatrix0 matrix3 (row - 1)
  in if row == 0 then matrix2 else recurse

solveMatrix :: Matrix -> [Float]
solveMatrix matrix = let
  (_, maxY, _) = matrix
  echelonForm = descendMatrix0 matrix maxY
  solution = backSubstitute echelonForm
  in solution

testMatrix = matrixFromRows [[-2, 1, -1], [1,1,6]]

convergePartA :: (Position, Velocity) -> (Position, Velocity) -> Maybe (Float, Float)
convergePartA ((x1, y1, _), (vx1, vy1, _)) ((x2, y2, _), (vx2, vy2, _)) = let
  -- vx1a + (-1 * vx2)b = x2 - x1
  -- vy1a + (-1 * vy2)b = y2 - y1
  matrix = matrixFromRows [[vx1, -1.0 * vx2, x2 - x1], [vy1, -1.0 * vy2, y2 - y1]]
  times = solveMatrix matrix
  time1 = traceShow times $ head times
  xConvergence = x1 + (vx1 * time1)
  yConvergence = y1 + (vy1 * time1)
  in if all (>= 0.0) times then Just (xConvergence, yConvergence) else Nothing

checkPartA :: ((Position, Velocity), (Position, Velocity)) -> Bool
checkPartA (pv1, pv2) = let
  minPos = 200000000000000.0
  maxPos = 400000000000000.0
  in case convergePartA pv1 pv2 of
    Nothing -> False
    Just (x, y) -> x >= minPos && x <= maxPos && y >= minPos && y <= maxPos

-- partA :: Input -> OutputA
partA input = let
  allPairs = [(x,y) | (x:ys) <- tails input, y <- ys]
  in length $ filter checkPartA allPairs

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
