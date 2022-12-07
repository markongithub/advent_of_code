module Day08 where

import Data.Char (digitToInt)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Coords = (Int, Int)
type Grid = Map Coords Int

parseLine :: String -> [Int]
parseLine = map digitToInt

rowToCoords :: (Int, [Int]) -> [(Coords, Int)]
rowToCoords (rowIndex, digits) = let
  columnsWithData :: [(Int, Int)]
  columnsWithData = zip [0..] digits
  makeCoords :: (Int, Int) -> (Coords, Int)
  makeCoords (c, d) = ((c, rowIndex), d)
  in map makeCoords columnsWithData

parseGrid :: [String] -> Grid
parseGrid ls = let
  linesAsDigits = map parseLine ls
  rowIndices = reverse [0..(length ls - 1)]
  linesAsDigitsWithRows :: [(Int, [Int])]
  linesAsDigitsWithRows = zip rowIndices linesAsDigits
  finalTuples :: [(Coords, Int)]
  finalTuples = concat $ map rowToCoords linesAsDigitsWithRows
  in Map.fromList finalTuples

testInput = parseGrid [
      "30373"
    , "25512"
    , "65332"
    , "33549"
    , "35390"
  ]

visibleAsWeMove :: Grid -> (Coords -> Coords) -> Set Coords -> Int -> Coords -> Set Coords
visibleAsWeMove grid func accu highestSoFar coords = let
  newCoords = func coords
  newAccu = Set.insert coords accu
  in case Map.lookup coords grid of
    Nothing -> accu -- these coords are not in the grid, we have finished a row/columnsWithData
    Just 9 -> newAccu -- we can quit looking because we won't find more
    Just h -> if h <= highestSoFar
                then visibleAsWeMove grid func accu highestSoFar newCoords
                else visibleAsWeMove grid func newAccu h newCoords

maxColumn :: Grid -> Int
maxColumn g = fst $ fst $ Map.findMax g

maxRow :: Grid -> Int
maxRow g = snd $ fst $ Map.findMax g

moveRight (c, r) = (c + 1, r)
visibleFromLeft :: Grid -> Set Coords
visibleFromLeft grid = let
  startingCoords = map (\r -> (0, r)) [0..(maxRow grid)]
  in Set.unions $ map (visibleAsWeMove grid moveRight Set.empty (-1)) startingCoords

moveLeft (c, r) = (c - 1, r)
visibleFromRight :: Grid -> Set Coords
visibleFromRight grid = let
  startingCoords = map (\r -> (maxColumn grid, r)) [0..(maxRow grid)]
  in Set.unions $ map (visibleAsWeMove grid moveLeft Set.empty (-1)) startingCoords

moveDown (c, r) = (c, r - 1)
visibleFromAbove :: Grid -> Set Coords
visibleFromAbove grid = let
  startingCoords = map (\c -> (c, maxRow grid)) [0..(maxColumn grid)]
  in Set.unions $ map (visibleAsWeMove grid moveDown Set.empty (-1)) startingCoords

moveUp (c, r) = (c, r + 1)
visibleFromBelow :: Grid -> Set Coords
visibleFromBelow grid = let
  startingCoords = map (\c -> (c, 0)) [0..(maxColumn grid)]
  in Set.unions $ map (visibleAsWeMove grid moveUp Set.empty (-1)) startingCoords

allVisible :: Grid -> Set Coords
allVisible grid = Set.unions [visibleFromLeft grid, visibleFromRight grid, visibleFromAbove grid, visibleFromBelow grid]

solvePart1 = do
  text <- readFile "data/input08.txt"
  return $ Set.size $ allVisible $ parseGrid $ lines text

lookInDirection :: Grid -> (Coords -> Coords) -> Set Coords -> Int -> Coords -> Set Coords
lookInDirection grid func accu myHeight coords = let
  newCoords = func coords
  newAccu = Set.insert newCoords accu
  in case Map.lookup newCoords grid of
    Nothing -> accu -- these coords are not in the grid, we have finished a row/columnsWithData
    Just 9 -> newAccu -- we can quit looking because we won't find more
    Just h -> if h < myHeight
                then lookInDirection grid func newAccu myHeight newCoords
                else newAccu

allVisibleFromCoords :: Grid -> Coords -> [Set Coords]
allVisibleFromCoords grid coords = let
  myHeight = grid!coords
  lookingUp = lookInDirection grid moveUp Set.empty myHeight coords
  lookingDown = lookInDirection grid moveDown Set.empty myHeight coords
  lookingLeft = lookInDirection grid moveLeft Set.empty myHeight coords
  lookingRight = lookInDirection grid moveRight Set.empty myHeight coords
  in  [lookingUp, lookingDown, lookingLeft, lookingRight]

scenicScore :: Grid -> Coords -> Int
scenicScore g c = product $ map Set.size $ allVisibleFromCoords g c

maxScenicScore :: Grid -> Int
maxScenicScore g = let
  allCoords = Map.keys g
  allScores = map (scenicScore g) allCoords
  in maximum allScores

solvePart2 = do
  text <- readFile "data/input08.txt"
  return $ maxScenicScore $ parseGrid $ lines text
