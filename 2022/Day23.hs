module Day23 where

-- import Common
import Data.Char (digitToInt)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set

type Coords = (Int, Int)
type Grid = Set Coords
type ProposableDirection = Coords -> (Coords,  [Coords])

proposeInDirection :: Grid -> Coords -> ProposableDirection -> Maybe Coords
proposeInDirection grid c func = let
  (dest, cellsToCheck) = func c
  canMove = all (\x -> Set.notMember x grid) cellsToCheck
  in if canMove then Just dest else Nothing

north (x, y) = ((x, y + 1), [(x-1, y+1), (x, y+1), (x+1,y+1)])
south (x, y) = ((x, y - 1), [(x-1, y-1), (x, y-1), (x+1,y-1)])
west  (x, y) = ((x - 1, y), [(x-1, y-1), (x-1,y), (x-1,y+1)])
east  (x, y) = ((x + 1, y), [(x+1, y-1), (x+1,y), (x+1,y+1)])
allAroundMe (x, y) = [(x-1, y+1), (x, y+1), (x+1,y+1),
                      (x-1, y),             (x+1, y),
                      (x-1, y-1), (x, y-1), (x+1,y-1)
                      ]
proposeFourDirections :: Grid -> [ProposableDirection] -> Coords -> Coords
proposeFourDirections grid directions c = let
  allEmptyAroundMe = all (\x -> Set.notMember x grid) (allAroundMe c)
  proposals = catMaybes $ map (proposeInDirection grid c) directions
  in if allEmptyAroundMe then c else case proposals of
    (x:xs) -> x
    []     -> c

parseLine :: String -> [Int]
parseLine s = let
  withIndices = zip s [0..]
  elfIndices = filter (\(char, _) -> char == '#') withIndices
  in map snd elfIndices

parseGrid :: [String] -> Set Coords
parseGrid ss = let
  elvesByLine = map parseLine ss
  numElves = length ss
  lineIndices = reverse $ take numElves [0..]
  elvesByRow :: [(Int, [Int])]
  elvesByRow = zip lineIndices elvesByLine
  makeRowCoords :: (Int, [Int]) -> [Coords]
  makeRowCoords (r, cs) = map (\c -> (c, r)) cs
  allCoords = map makeRowCoords elvesByRow
  in Set.fromList $ concat allCoords

testInput = [
    "....#.."
  , "..###.#"
  , "#...#.#"
  , ".#...##"
  , "#.###.."
  , "##.#.##"
  , ".#..#.."
  ]

testInput2 = [
    "....."
  , "..##."
  , "..#.."
  , "....."
  , "..##."
  , "....."
  ]

directionsByTurn :: Int -> [ProposableDirection]
directionsByTurn turn = let
  setOfEight = [north, south, west, east, north, south, west, east]
  toDrop = (turn - 1) `mod` 4
  in take 4 $ drop toDrop setOfEight

allProposals :: Grid -> Int -> [(Coords, Coords)]
allProposals grid turn = let
  directions = directionsByTurn turn
  elves = Set.toList grid
  proposals = map (proposeFourDirections grid directions) elves
  in zip elves proposals

type ProposalMap = Map Coords (Set Coords)
makeProposalMap :: [(Coords, Coords)] -> ProposalMap
makeProposalMap ls = let
  insertFunc oldMap (from, to) = Map.insertWith Set.union to (Set.singleton from) oldMap
  in foldl insertFunc Map.empty ls

newGridAfterMoves :: ProposalMap -> Grid
newGridAfterMoves pMap = let
  newElvesByKey key = case Set.size (pMap!key) of
    1 -> [key]
    _ -> Set.toList (pMap!key)
  allNewLocations = concat $ (map newElvesByKey) $ Map.keys pMap
  in Set.fromList allNewLocations

runTurn :: Grid -> Int -> Grid
runTurn grid turn = newGridAfterMoves $ makeProposalMap $ allProposals grid turn

runNTurns :: Grid -> Int -> Grid
runNTurns grid turns = foldl runTurn grid [1..turns] 

runUntilStable :: Grid -> Int -> (Grid, Int)
runUntilStable grid turn = let
  grid2 = runTurn grid turn
  in if grid == grid2 then (grid, turn) else runUntilStable grid2 (turn + 1)

minimumRectangle :: Grid -> (Coords, Coords)
minimumRectangle grid = let
  allElves = Set.toList grid
  allXes = map fst allElves
  allYes = map snd allElves
  in ((minimum allXes, minimum allYes), (maximum allXes, maximum allYes))

rectangleArea :: (Coords, Coords) -> Int
rectangleArea ((x1, y1), (x2, y2)) = (x2 + 1 - x1) * (y2 + 1 - y1)

emptyGroundTiles :: Grid -> Int
emptyGroundTiles grid = let
  minimumRectangleArea = rectangleArea $ minimumRectangle grid
  in minimumRectangleArea - Set.size grid

solvePart1Pure :: [String] -> Int
solvePart1Pure ss = emptyGroundTiles $ runNTurns (parseGrid ss) 10

solvePart1 = do
  text <- readFile "data/input23.txt"
  return $ solvePart1Pure $ lines text

solvePart2Pure :: [String] -> Int
solvePart2Pure ss = snd $ runUntilStable (parseGrid ss) 1

solvePart2 = do
  text <- readFile "data/input23.txt"
  return $ solvePart2Pure $ lines text