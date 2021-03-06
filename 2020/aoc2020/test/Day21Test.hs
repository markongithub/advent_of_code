import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Day21

makeTest expected actual = testCase [] (assertEqual [] expected actual)

day21PureTests = [ makeTest (Set.singleton $ Set.fromList ["sqjhc", "fvjkl"]) (day21FullMap!"soy")
                 , makeTest 2 (length $ day21FullMap!"fish")
                 , makeTest (Possibilities $ Set.singleton "mxmxvkd") (day21ReducedMap!"dairy")
                 , makeTest False (isSolved day21ReducedMap)
                 , makeTest ("dairy", "mxmxvkd") (findSingleIngredient day21ReducedMap)
                 , makeTest (Possibilities (Set.singleton "sqjhc")) ((solveAllergen day21ReducedMap "dairy" "mxmxvkd")!"fish")
                 , makeTest 5 (solvePart1Func day21TestInput)
                 , makeTest "mxmxvkd,sqjhc,fvjkl" (solvePart2Func day21TestInput)
                 ]

main = do
  day21Solution1 <- solvePart1
  let day21Solution1Test = testCase [] (assertEqual [] 2436
                                        day21Solution1)
  day21Solution2 <- solvePart2
  let day21Solution2Test =
        testCase [] (
          assertEqual [] "dhfng,pgblcd,xhkdc,ghlzj,dstct,nqbnmzx,ntggc,znrzgs"
            day21Solution2)
  defaultMain $ testGroup [] (day21PureTests ++ [
    day21Solution1Test, day21Solution2Test])
