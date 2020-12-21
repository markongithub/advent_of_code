import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Day21

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day21PureTests = [ makeTest (Set.singleton $ Set.fromList ["sqjhc", "fvjkl"]) (day21TestMap!"soy")
                 , makeTest 2 (length $ day21TestMap!"fish")
                 , makeTest False (isSolved day21TestMap)
                 ]

main = do
  -- day21Solution1 <- solvePart1 "data/input21.txt"
  -- let day21Solution1Test = testCase [] (assertEqual [] 142
  --                                       day21Solution1)
  defaultMain $ testGroup [] (day21PureTests) --  ++ [
  --  day21Solution1Test, part2Test1, part2Test2, day21Solution2Test])
