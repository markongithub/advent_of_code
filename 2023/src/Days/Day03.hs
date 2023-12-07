module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (isDigit)
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
parseRow0 :: Int -> String -> Int -> Input -> Input
parseRow0 _ [] _ accu = accu
parseRow0 y (c:cs) x (numbers, symbols)
  | isDigit c = handleDigit
  | c == '.'  = handleDot
  | otherwise = handleSymbol
  where
    allDigits = c:(Data.List.takeWhile isDigit cs)
    afterDigits = dropWhile isDigit cs
    newNumbers = (allDigits, (x, y)):numbers
    xAfterDigits = x + length allDigits
    handleDigit = parseRow0 y afterDigits xAfterDigits (newNumbers, symbols)
    newSymbols = (c, (x,y)):symbols
    handleSymbol = parseRow0 y cs (x + 1) (numbers, newSymbols)
    handleDot = parseRow0 y cs (x + 1) (numbers, symbols)

parseRow :: (Int, String) -> Input
parseRow (y, s) = let
  initialAccu = ([], [])
  in parseRow0 y s 0 initialAccu

parseRows :: [String] -> Input
parseRows ss = let
  rowsWithIndices = zip [0..] ss
  parsedRows :: [Input]
  parsedRows = map parseRow rowsWithIndices
  initialAccu = ([], [])
  in foldl mergeInputLines initialAccu parsedRows 

mergeInputLines :: Input -> Input -> Input
mergeInputLines (xs1, ys1) (xs2, ys2) = (xs1 ++ xs2, ys1 ++ ys2)

inputParser :: Parser Input
inputParser = do
  rowStrs <- many1 (notChar '\n') `sepBy` endOfLine
  return $ parseRows rowStrs

------------ TYPES ------------
type Coords = (Int, Int)
type SchematicNumber = (String, Coords)
type Symbol = (Char, Coords)
type Input = ([SchematicNumber], [Symbol])

type OutputA = Int

type OutputB = Int

------------ PART A ------------

coordsAroundNumber :: SchematicNumber -> [Coords]
coordsAroundNumber (s, (x2, y2)) = let
  xRange = [(x2 - 1)..(x2 + length s)]
  yRange = [y2 - 1, y2, y2+1]
  allCandidates = [ (x,y) | x<-xRange, y<-yRange ]
  in allCandidates

isNearCoords :: Coords -> SchematicNumber -> Bool
isNearCoords  (x1, y1) (s, (x2, y2)) = let
  allCandidates = coordsAroundNumber (s, (x2, y2))
  in elem (x1, y1) allCandidates

isNearSymbol :: [Symbol] -> SchematicNumber -> Bool
isNearSymbol symbols number = let
  allCoords = map snd symbols
  in any (\c -> isNearCoords c number) allCoords

partA :: Input -> Int
partA (numbers, symbols) = let
  partNumbers = filter (isNearSymbol symbols) numbers
  in sum $ map (read . fst) partNumbers

------------ PART B ------------

type CoordsToNumbers = Map Coords [String]
insertNumber :: CoordsToNumbers -> SchematicNumber -> CoordsToNumbers
insertNumber oldMap (str, coords) = let
  allCoords = coordsAroundNumber (str, coords)
  insertOne :: CoordsToNumbers -> Coords -> CoordsToNumbers
  insertOne m c = Map.insertWith (++) c [str] m
  in foldl insertOne oldMap allCoords

makeNumberMap :: [SchematicNumber] -> CoordsToNumbers
makeNumberMap numbers = foldl insertNumber Map.empty numbers

numbersNearCoords :: CoordsToNumbers -> Coords -> [Int]
numbersNearCoords cMap c = map read $ Map.findWithDefault [] c cMap

partB :: Input -> OutputB
partB (numbers, symbols) = let
  gearCoords = map snd $ filter (\s -> fst s == '*') symbols
  numberMap = makeNumberMap numbers
  numbersByGear = map (numbersNearCoords numberMap) gearCoords
  realGears :: [[Int]]
  realGears = filter (\l -> length l == 2) numbersByGear
  in sum $ map product realGears