module Day21 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Node = Start | BigCave String | SmallCave String | End
  deriving (Eq, Ord, Show)

