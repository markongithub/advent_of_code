import Test.Tasty (defaultMain, testGroup)

import Day12Test
import Day14Test
import Day15Test

allTests = concat [
    day12Tests
  , day14Tests
  , day15Tests
  ]

main = do
  defaultMain $ testGroup [] allTests
