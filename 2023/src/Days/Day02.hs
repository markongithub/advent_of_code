module Days.Day02 (runDay) where

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

-- Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
cubeCountParser :: Parser (Int, String)
cubeCountParser = do
  many' space
  countStr <- many1 digit
  many' space
  colorStr <- many1 anyChar
  return (read countStr, colorStr)

cubeSetParser :: Parser [(Int, String)]
cubeSetParser = cubeCountParser `sepBy` char ','

gameParser :: Parser Game
gameParser = cubeSetParser `sepBy` char ';'

inputParser :: Parser Input
inputParser = gameParser `sepBy` endOfLine

------------ TYPES ------------

type CubeCount = (Int, String)
type CubeSet = [CubeCount]
type Game = [CubeSet]
type Input = [Game]

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
