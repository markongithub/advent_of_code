import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day13

day13TestTimestamp = "939"
day13TestBusLine1 = "7,13,x,x,59,x,31,19"

day13Test1 = testCase []
  (assertEqual [] 295 (solvePart1Pure day13TestTimestamp day13TestBusLine1))

testPart2 (input, expected) = testCase [] (assertEqual [] expected (solvePart2Pure input))

buses = parseBusLine "7,x,13"
sortedBuses = sortByFstDesc buses
maxBus = head sortedBuses
otherBuses = tail sortedBuses

testOffsets = map busToSaneOffset [(661,44),(641,13),(41,3),(37,50),(29,42),(23,67),(19,25),(17,30),(13,0)]

purePart2Tests = [
    testCase [] (assertEqual [] [(13,2),(7,0)] (parseBusLine "7,x,13"))
  , testCase [] (assertEqual [] (13,2) maxBus)
  , testCase [] (assertEqual [] [(7,0)] otherBuses)
  , testCase [] (assertEqual [] (13,2) (head $ sortByFstDesc $ parseBusLine "7,x,13"))
  , testCase [] (assertEqual [] 63 (earliestCorrectTimestamp $ parseBusLine "7,x,13"))
  , testCase [] (assertEqual [] (91,76) (mergeTwoOffsets (13,11) (7,6)))
  , testPart2 ("7,13", 77)
  , testPart2 ("7,x,13", 63)
  , testPart2 (day13TestBusLine1, 1068781)
  , testPart2 ("67,7,59,61", 754018)
  , testPart2 ("67,x,7,59,61", 779210)
  , testPart2 ("67,7,x,59,61", 1261476)
  , testPart2 ("1789,37,47,1889", 1202161486)
  , testCase [] (assertEqual [] (37,24) (busToSaneOffset (37,50)))
  ]

main = do
  day13Solution1 <- solvePart1
  let day13Solution1Test = testCase [] (assertEqual [] 3966 day13Solution1)
  day13Solution2 <- solvePart2
  let day13Solution2Test = testCase [] (assertEqual []
                                        800177252346225 day13Solution2)
  defaultMain $ testGroup []
    ([day13Test1, day13Solution1Test] ++ purePart2Tests ++ [day13Solution2Test])
