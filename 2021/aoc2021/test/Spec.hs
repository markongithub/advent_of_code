import Test.Tasty (defaultMain, testGroup)

import Day12Test
import Day14Test
import Day15Test
import Day16Test
import Day21Test

allTests = concat [
    day12Tests
  , day14Tests
  , day15Tests
  , day16Tests
  , day21Tests
  ]

main = do
  defaultMain $ testGroup [] allTests
