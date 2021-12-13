import Test.Tasty (defaultMain, testGroup)

import Day12Test
import Day14Test

allTests = concat [
    day12Tests
  , day14Tests
  ]

main = do
  defaultMain $ testGroup [] allTests
