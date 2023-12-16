module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (ord)
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
inputParser :: Parser Input
inputParser = do
  strs <- many1 (satisfy $ notInClass ",\n") `sepBy` char ','
  return strs

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

updateHash :: Int -> Char -> Int
updateHash val0 chr = let
  asciiVal = ord chr
  val1 = val0 + asciiVal
  val2 = val1 * 17
  val3 = val2 `mod` 256
  in val3

hash :: String -> Int
hash str = foldl updateHash 0 str

partA :: Input -> OutputA
partA input = sum $ map hash input

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
