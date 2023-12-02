module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Control.Applicative

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

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

colorParser :: Parser Color
colorParser =
     (string "red"   >> return Red)
 <|> (string "green" >> return Green)
 <|> (string "blue"  >> return Blue)

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
cubeCountParser :: Parser (Int, Color)
cubeCountParser = do
  many' space
  countStr <- many1 digit
  many' space
  color <- colorParser
  return (read countStr, color)

cubeSetParser :: Parser [(Int, Color)]
cubeSetParser = cubeCountParser `sepBy` char ','

gameParser :: Parser Game
gameParser = do
  string "Game "
  gameNumStr <- many1 digit
  string ": "
  gameData <- cubeSetParser `sepBy` char ';'
  return (read gameNumStr, gameData)

inputParser :: Parser Input
inputParser = gameParser `sepBy` endOfLine

------------ TYPES ------------

type CubeCount = (Int, Color)
type CubeSet = [CubeCount]
type Game = (Int, [CubeSet])
type Input = [Game]

type OutputA = Int

type OutputB = Void

------------ PART A ------------

type ColorCount = Map Color Int

updateColorCount :: ColorCount -> CubeCount -> ColorCount
updateColorCount oldMap (count, color) = let
  oldValue = Map.findWithDefault 0 color oldMap
  in Map.insert color (max oldValue count) oldMap

colorCountForGame :: Game -> ColorCount
colorCountForGame (_, cubes) = let
  allCounts :: [CubeCount]
  allCounts = concat cubes
  in foldl updateColorCount Map.empty allCounts

gameQualifies :: Game -> Bool
gameQualifies game = let
  colorCount = colorCountForGame game
  in ((Map.findWithDefault 0 Red colorCount <= 12) &&
      (Map.findWithDefault 0 Green colorCount <= 13) &&
      (Map.findWithDefault 0 Blue colorCount <= 14))

partA :: Input -> OutputA
partA input = sum $ map fst $ filter gameQualifies input
  

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
