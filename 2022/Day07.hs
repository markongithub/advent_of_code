module Day07 where

import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Statement = ChDir String | LS | FileSize Int String | DirName String
                 deriving (Eq, Ord, Show)


splitAtSep :: String -> Char -> (String, String)
splitAtSep s sep = let
  firstPart = takeWhile (/= sep) s
  secondPart = drop (length firstPart + 1) s
  in (firstPart, secondPart)

parseStatement :: String -> Statement
parseStatement str = let
  parseCommand commandStr = let
    command = take 2 commandStr
    in case command of
      "ls" -> LS
      "cd" -> ChDir (drop 3 commandStr)
  parseFileSize s = let
    (sizeStr, fileName) = splitAtSep s ' '
    in FileSize (read sizeStr :: Int) fileName
  in case (head str) of
    '$' -> parseCommand (drop 2 str)
    'd' -> DirName $ snd $ splitAtSep str ' '
    _   -> parseFileSize str

testInput = map parseStatement [
    "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  , "$ cd .."
  ]

data TraversalState = TraversalState {
                        getCurrentPath :: [String]
                      , getSizeMap :: Map [String] Int
                        } deriving Show

applyStatement :: TraversalState -> Statement -> TraversalState
applyStatement (TraversalState curPath sizeMap) statement = let
  doNothing = TraversalState curPath sizeMap
  curSize = Map.findWithDefault 0 curPath sizeMap
  parentPath = tail curPath
  in case statement of
  DirName _ -> doNothing
  LS -> doNothing
  ChDir ".." -> TraversalState parentPath $ Map.insertWith (+) parentPath curSize sizeMap
  ChDir newDir -> TraversalState (newDir:curPath) sizeMap
  FileSize size _  -> TraversalState curPath $ Map.insertWith (+) curPath size sizeMap

initialState = TraversalState [] Map.empty

-- this doesn't work at all unless you manually add a bunch of "cd .." to your input. But that's programming too!
solvePart1Pure :: [Statement] -> Int
solvePart1Pure statements = sum $ filter (<= 100000) $ map snd $ Map.toList $ getSizeMap $ foldl applyStatement initialState statements

solvePart1 :: IO Int
solvePart1 = do
  text <- readFile "data/input07.txt"
  return $ solvePart1Pure $ map parseStatement $ lines text

debugPart1 :: IO [Statement]
debugPart1 = do
  text <- readFile "data/input07.txt"
  return $ map parseStatement $ lines text

solvePart2Pure :: TraversalState -> Int
solvePart2Pure (TraversalState _ sizeMap) = let
  totalDiskUsed = sizeMap!(["/"])
  diskSpaceFree = 70000000 - totalDiskUsed
  neededSpace = 30000000 - diskSpaceFree
  allFreeSpaces = Map.elems sizeMap
  in head $ sort $ filter (>= neededSpace) allFreeSpaces

solvePart2 :: IO Int
solvePart2 = do
  statements <- debugPart1
  return $ solvePart2Pure $ foldl applyStatement initialState statements