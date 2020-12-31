import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Data.Map (Map, (!))
import qualified Data.Map as Map

import Day24

makeTest expected actual = testCase [] (assertEqual [] expected actual)
day24TestInput = [
    "sesenwnenenewseeswwswswwnenewsewsw"
  , "neeenesenwnwwswnenewnwwsewnenwseswesw"
  , "seswneswswsenwwnwse"
  , "nwnwneseeswswnenewneswwnewseswneseene"
  , "swweswneswnenwsewnwneneseenw"
  , "eesenwseswswnenwswnwnwsewwnwsene"
  , "sewnenenenesenwsewnenwwwse"
  , "wenwwweseeeweswwwnwwe"
  , "wsweesenenewnwwnwsenewsenwwsesesenwne"
  , "neeswseenwwswnwswswnw"
  , "nenwswwsewswnenenewsenwsenwnesesenew"
  , "enewnwewneswsewnwswenweswnenwsenwsw"
  , "sweneswneswneneenwnewenewwneswswnese"
  , "swwesenesewenwneswnwwneseswwne"
  , "enesenwswwswneneswsenwnewswseenwsese"
  , "wnwnesenesenenwwnenwsewesewsesesew"
  , "nenewswnwewswnenesenwnesewesw"
  , "eneswnwswnwsenenwnwnwwseeswneewsenese"
  , "neswnwewnwnwseenwseesewsenwsweewe"
  , "wseweeenwnesenwwwswnew"
  ]

day24PureTests = [ makeTest 10 (solvePart1Func day24TestInput)
                 , makeTest (-3,1) (sumPairs $ parseDirections "nww")
                 , makeTest (-4,0) (sumPairs $ parseDirections "nwwsw")
                 , makeTest (0,0) (sumPairs $ parseDirections "nwwswee")
                 , makeTest 15 (solvePart2Func 1 day24TestInput)
                 , makeTest 2208 (solvePart2Func 100 day24TestInput)
                 ]

main = do
  day24Solution1 <- solvePart1
  let day24Solution1Test = testCase [] (assertEqual [] 465
                                        day24Solution1)
  day24Solution2 <- solvePart2
  let day24Solution2Test = testCase [] (assertEqual [] 4078
                                        day24Solution2)
  defaultMain $ testGroup [] (day24PureTests ++
     [day24Solution1Test, day24Solution2Test]) -- , day24Solution2Test])
