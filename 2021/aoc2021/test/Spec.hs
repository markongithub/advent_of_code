import Test.Tasty (defaultMain, testGroup)

import Day12Test

main = do
  defaultMain $ testGroup [] day12Tests
