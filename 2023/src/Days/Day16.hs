module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
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
parseRow :: (Int, String) -> [(Coords, Char)]
parseRow (y, s) = let
  withXCoords :: [(Int, Char)]
  withXCoords = zip [0..] s
  toCoordsPair (x, c) = ((x, y), c)
  in map toCoordsPair withXCoords

parseRows :: [String] -> Input
parseRows ss = let
  rowIndices = reverse [0..(length ss - 1)]
  rowsWithIndices = zip rowIndices ss
  parsedRows = concat $ map parseRow rowsWithIndices
  in Map.fromList $ filter (\(_, c) -> c /= '.') parsedRows

inputParser :: Parser Input
inputParser = do
  rowStrs <- many1 (notChar '\n') `sepBy` endOfLine
  return (parseRows rowStrs)

------------ TYPES ------------
type Coords = (Int, Int)
type Input = Map Coords Char
data Direction = North | South | West | East deriving (Eq, Ord, Show)


type OutputA = Int

type OutputB = Int

------------ PART A ------------

move :: Coords -> Direction -> Coords
move (x,y) dir = case dir of
  North -> (x, y + 1)
  South -> (x, y - 1)
  West -> (x - 1, y)
  East -> (x + 1, y)

inBounds :: Coords -> Coords -> Bool
inBounds (maxX, maxY) (x, y) = x >=0 && y >=0 && x <= maxX && y <= maxY

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite West = East
opposite East = West

hitMirror :: Direction -> Char -> Direction
hitMirror North '/' = East
hitMirror East '/' = North
hitMirror South '/' = West
hitMirror West '/' = South
hitMirror dir '\\' = opposite $ hitMirror dir '/'
hitMirror dir c = error ("bad mirror: " ++ [c])

hitSplitter :: Direction -> Char -> [Direction]
hitSplitter d c = let
  firstDir North '|' = Nothing
  firstDir North '-' = Just West
  firstDir South c2 = firstDir North c2
  firstDir West '|' = Just North
  firstDir West '-' = Nothing
  firstDir East c2 = firstDir West c2
  firstDir d2 c2 = error ("unexpected splitter: " ++ show (d2, c2))
  in case firstDir d c of
    Nothing -> [d]
    Just dOut -> [dOut, opposite dOut]

isMirror :: Char -> Bool
isMirror '/' = True
isMirror '\\' = True
isMirror _ = False

isSplitter :: Char -> Bool
isSplitter '-' = True
isSplitter '|' = True
isSplitter _ = False

trackBeam :: Input -> Coords -> Set (Coords, Direction) -> (Coords, Direction) -> Set (Coords, Direction)
trackBeam graph bounds explored (coords, dir) = let
  thisCell = Map.findWithDefault '.' coords graph
  nextDirections = case thisCell of
    '.' -> [dir]
    '/' -> [hitMirror dir thisCell]
    '\\' -> [hitMirror dir thisCell]
    '-' -> hitSplitter dir thisCell
    '|' -> hitSplitter dir thisCell
    x -> error ("bad cell content: " ++ [x])
  candidates = map (move coords) nextDirections
  candidatePairs :: [(Coords, Direction)]
  candidatePairs = zip candidates nextDirections
  destinations = filter (\(c1, d1) -> inBounds bounds c1) candidatePairs
  goDest e1 (c1, d1) = trackBeam graph bounds e1 (c1,d1)
  newExplored = Set.insert (coords, dir) explored
  goAllDests = foldl goDest newExplored destinations
  in case Set.member (coords, dir) explored of
    True -> explored
    False -> goAllDests

findBounds :: Input -> Coords
findBounds input = let
  allCoords = Map.keys input
  (allX, allY) = unzip allCoords
  in (maximum allX, maximum allY)

partA :: Input -> OutputA
partA input = let
  bounds = findBounds input
  firstCoords = (0, snd bounds)
  in Set.size $ Set.fromList $ map fst $ Set.toList $ trackBeam input bounds Set.empty (firstCoords, East)


------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
