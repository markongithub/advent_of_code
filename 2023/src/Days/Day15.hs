module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Char (ord, isLetter)
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
inputParser = many1 (satisfy $ notInClass ",\n") `sepBy` char ','

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
type Lens = (String, Int)
type Box = [Lens]
type Boxes = Map Int Box

addLens :: Box -> Lens -> Box
addLens [] lens = [lens]
addLens ((l1, fl1):xs) (l2, fl2) = case l1 == l2 of
  True -> (l2, fl2):xs
  False -> ((l1, fl1):(addLens xs (l2, fl2)))

filterOutOne :: (a -> Bool) -> [a] -> [a]
filterOutOne func [] = []
filterOutOne func (x:xs) = case func x of
  True -> xs
  False -> (x:(filterOutOne func xs))

removeLens :: Box -> String -> Box
removeLens box label = filterOutOne (\(lab, fl) -> lab == label) box

doCommand :: Boxes -> String -> Boxes
doCommand boxes str = let
  label = Data.List.takeWhile isLetter str
  remainder = drop (length label) str
  boxNum = hash label
  box = Map.findWithDefault [] boxNum boxes
  newBox = case remainder of
    ('=':fl) -> addLens box (label, read fl)
    "-" -> removeLens box label
    _ -> error ("unexpected command: " ++ str)
  in Map.insert boxNum newBox boxes

lensesInBox :: Box -> [(Lens, Int)]
lensesInBox box = zip box [1..]

lensesByBox :: Boxes -> [(Lens, Int, Int)]
lensesByBox boxes = let
  tuples = Map.toList boxes
  lensPairToTriple :: Int -> (Lens, Int) -> (Lens, Int, Int)
  lensPairToTriple boxNum (lens, slot) = (lens, slot, boxNum + 1)
  listOfTriples :: (Int, Box) -> [(Lens, Int, Int)]
  listOfTriples (boxNum, box) = map (lensPairToTriple boxNum) $ lensesInBox box
  in concat $ map listOfTriples tuples

focalPower :: (Lens, Int, Int) -> Int
focalPower ((_, fl),slotNum,boxNumPlus) = fl * slotNum * boxNumPlus

partB :: Input -> OutputB
partB input = let
  finalBoxes = foldl doCommand Map.empty input
  in sum $ map focalPower $ lensesByBox finalBoxes