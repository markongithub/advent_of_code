module Day13 where

import Data.List (minimumBy, sortBy)

waitForBus :: Int -> Int -> Int
waitForBus timestamp bus = let
  remainder = timestamp `mod` bus
  in case remainder of
    0 -> 0
    _ -> bus - remainder

earliestBusTimesWait :: Int -> [Int] -> Int
earliestBusTimesWait timestamp buses = let
  waitWithID bus = (waitForBus timestamp bus, bus)
  waitsWithIDs :: [(Int, Int)]
  waitsWithIDs = map waitWithID buses
  compareFst a b = compare (fst a) (fst b)
  bestID :: Int
  (bestWait, bestID) = minimumBy compareFst waitsWithIDs
  in bestWait * bestID

-- plagiarizing from my own 2019d03 solution
splitOnCommas0 :: String -> [String] -> [String]
splitOnCommas0 str accu = case str of
  [] -> reverse accu
  (',':xs) -> splitOnCommas0 xs accu
  _ -> let
    nextSegment = takeWhile (/= ',') str
    remainder = dropWhile (/= ',') str
    newAccu = nextSegment:accu
    in splitOnCommas0 remainder newAccu

splitOnCommas :: String -> [String]
splitOnCommas str = splitOnCommas0 str []

parseBuses0 :: [String] -> Int -> [(Int, Int)] -> [(Int, Int)]
parseBuses0 [] _ accu = accu
parseBuses0 (x:xs) i accu = case x of
  "x" -> parseBuses0 xs (i+1) accu
  bus -> parseBuses0 xs (i+1) (((read bus :: Int), i):accu)

parseBuses :: [String] -> [(Int, Int)]
parseBuses xs = parseBuses0 xs 0 []

parseBusLine :: String -> [(Int, Int)]
parseBusLine str = parseBuses $ splitOnCommas str

solvePart1Pure :: String -> String -> Int
solvePart1Pure timestampStr busLine = let
  timestamp = read timestampStr :: Int
  buses = map fst $ parseBusLine busLine
  in earliestBusTimesWait timestamp buses

solvePart1 :: IO Int
solvePart1 = do
  text <- readFile "data/input13.txt"
  let [timestampStr, busLine] = lines text
  return $ solvePart1Pure timestampStr busLine

sortByFstDesc :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFstDesc ls = sortBy (\(d1, _) (d2, _) -> compare d2 d1) ls

checkTimestampForBus :: Int -> (Int, Int) -> Bool
checkTimestampForBus timestamp (busID, offset) =
  timestamp `mod` busID == ((busID - offset) `mod` busID)

checkTimestampForAllBuses :: [(Int, Int)] -> Int -> Bool
checkTimestampForAllBuses buses timestamp =
  all (checkTimestampForBus timestamp) buses

candidateTimestamps :: (Int, Int) -> [Int]
candidateTimestamps (busID, offset) = map (\k -> (busID - offset) + (busID * k)) [0..]

busToSaneOffset :: (Int, Int) -> (Int, Int)
busToSaneOffset (busID, offset) = let
  offset2 = offset `mod` busID
  offset3 = case offset2 of
    0 -> 0
    _ -> busID - offset2
  in (busID, offset3)

-- lists must be ordered
leastCommonElement :: (Ord a) => [a] -> [a] -> a
leastCommonElement (x:xs) (y:ys)
  | x == y    = x
  | x > y     = leastCommonElement (x:xs) (dropWhile (< x) (y:ys))
  | otherwise = leastCommonElement (dropWhile (< y) xs) (y:ys)

mergeTwoOffsets :: (Int, Int) -> (Int, Int) -> (Int, Int)
mergeTwoOffsets (a, x) (b, y) = let
  candidates = map (\k -> (k*a) + x) [0..b]
  selector c = c `mod` b == y
  newOffset = head $ filter selector candidates
  in (a * b, newOffset)

mergeOffsets :: [(Int, Int)] -> (Int, Int)
mergeOffsets xs = foldl mergeTwoOffsets (1, 0) xs

earliestCorrectTimestamp :: [(Int, Int)] -> Int
earliestCorrectTimestamp buses = let
  saneOffsets = map busToSaneOffset (sortByFstDesc buses)
  in snd $ mergeOffsets saneOffsets
  
--earliestCorrectTimestamp buses = let
--  sortedBuses = sortByFstDesc buses
--  maxBus = head sortedBuses
--  otherBuses = tail sortedBuses
--  correctTimestamps = filter (checkTimestampForAllBuses otherBuses) (candidateTimestamps maxBus)
--  in head correctTimestamps

solvePart2Pure :: String -> Int
solvePart2Pure busLine = earliestCorrectTimestamp $ parseBusLine busLine

solvePart2 :: IO Int
solvePart2 = do
  text <- readFile "data/input13.txt"
  let busLine = (lines text)!!1
  putStrLn (show $ sortByFstDesc $ parseBusLine busLine)
  return $ solvePart2Pure busLine

