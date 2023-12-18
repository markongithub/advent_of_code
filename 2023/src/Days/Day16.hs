module Days.Day16 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map, (!))
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
type Vertex = (Coords, Direction)

getNeighbors :: Input -> Coords -> Vertex -> [Vertex]
getNeighbors graph bounds (coords, dir) = let
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
  in filter (\(c1, d1) -> inBounds bounds c1) candidatePairs


data TarjanState = TarjanState {
    getLow :: Map Vertex Int
  , getDisc :: Map Vertex Int
  , getVisited :: Set Vertex
  , getNextIndex :: Int
  , getStack :: [Vertex]
  , getStackSet :: Set Vertex
  , getSCCs :: [[Vertex]]
  }

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore _ [] = []
takeWhileOneMore f (x:xs) = case f x of
  True -> x:(takeWhileOneMore f xs)
  False -> [x]

tarjan :: (Vertex -> [Vertex]) -> TarjanState -> Vertex -> TarjanState
tarjan neighbors state u = let
  newDisc = Map.insert u (getNextIndex state) $ getDisc state
  newIndex = 1 + getNextIndex state
  newStack = u:(getStack state)
  newStackSet = Set.insert u (getStackSet state)
  vs = neighbors u
  stateBeforeRecursions = state { getDisc = newDisc, getNextIndex = newIndex, getStack = newStack, getStackSet = newStackSet}
  maybeRecurseNeighbor :: TarjanState -> Vertex -> TarjanState
  maybeRecurseNeighbor s0 v = if Map.member v (getDisc s0) then s0 else tarjan neighbors s0 v
  stateAfterRecursions = foldl maybeRecurseNeighbor stateBeforeRecursions vs
  unvisited = filter (\v -> Map.notMember v (getDisc state)) vs
  visitedAndOnStack = filter (\v -> Map.member v (getDisc state) && Set.member v (getStackSet state)) vs
  candidateLows = [(getLow stateAfterRecursions)!u] ++ (map (\v -> (getLow stateAfterRecursions)!v) unvisited) ++ (map (\v -> (getDisc stateAfterRecursions)!v) visitedAndOnStack)
  newLowU = minimum candidateLows
  newDiscU = (getDisc stateAfterRecursions)!u
  thisSCC = if newLowU == newDiscU then takeWhileOneMore (/= u) (getStack stateAfterRecursions) else []
  finalStack = drop (length thisSCC) (getStack stateAfterRecursions)
  finalStackSet = foldl (flip Set.delete) (getStackSet stateAfterRecursions) thisSCC
  newSCCs = if null thisSCC then (getSCCs stateAfterRecursions) else thisSCC:(getSCCs stateAfterRecursions)
  in error "not yet"

partB :: Input -> OutputB
partB = error "Not implemented yet!"
