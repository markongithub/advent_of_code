import Test.Tasty (defaultMain, testGroup)

import Day12Test
import Day14Test
import Day15Test
import Day16Test

allTests = concat [
    day12Tests
  , day14Tests
  , day15Tests
  , day16Tests
  ]

main = do
  defaultMain $ testGroup [] allTests
