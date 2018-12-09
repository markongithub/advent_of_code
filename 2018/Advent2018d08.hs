module Main where

data Day8Node = Day8Node { children :: [Day8Node]
                         , metadata :: [Int] } deriving (Show)

parseNode :: [Int] -> Int -> ([Day8Node], [Int])
parseNode ints 0 = ([], ints)
parseNode ints nodes = let
  [numChildren, metadataSize] = take 2 ints
  (parsedChildren, remainder) = parseNode (drop 2 ints) numChildren
  parsedMetadata = take metadataSize remainder
  thisNode = Day8Node parsedChildren parsedMetadata
  (otherNodes, finalRemainder) =
    parseNode (drop metadataSize remainder) (nodes - 1)
  in ((thisNode:otherNodes), finalRemainder)

parseNodeStr :: String -> Day8Node
parseNodeStr str
  | length nodes > 1 = error "Got more than one node"
  | length nodes < 1 = error "Got less than one node"
  | length remainder > 0 = error "got a remainder"
  | otherwise = head nodes
  where (nodes, remainder) =
          parseNode (map (\w -> (read w :: Int)) $ words str) 1

solvePart1 :: Day8Node -> Int
solvePart1 (Day8Node nChildren nMetadata) =
  sum nMetadata + sum (map solvePart1 nChildren)

testStr = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

main :: IO ()
main = do
  putStrLn $ show $ solvePart1 $ parseNodeStr testStr
  text <- readFile "input/Advent2018d08.txt"
  putStrLn $ show $ solvePart1 $ parseNodeStr text
--  prereqs <- parseFile "input/Advent2018d07.txt"
--  putStrLn $ show $ solvePart1 prereqs
--  putStrLn $ show $ solvePart2 testData 2 0
--  putStrLn $ show $ solvePart2 prereqs 5 60
