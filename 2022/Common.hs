module Common where

splitAtSep :: String -> Char -> [String]
splitAtSep [] _ = []
splitAtSep s sep = let
  firstPart = takeWhile (/= sep) s
  in firstPart:(splitAtSep (drop (length firstPart + 1) s) sep)

data Queue a = Queue {
    getFront :: [a]
  , getRear :: [a]
} deriving Show

newQueue = Queue [] []

emptyQ :: Queue a -> Bool
emptyQ (Queue [] []) = True
emptyQ _ = False

pushQ :: Queue a -> a -> Queue a
pushQ (Queue front back) newItem = Queue front (newItem:back)

popQ :: Queue a -> (a, Queue a)
popQ (Queue [] []) = error "You should have called emptyQ first."
popQ (Queue (x:xs) back) = (x, Queue xs back)
popQ (Queue [] back) = let
  (x:xs) = reverse back
  in (x, Queue xs [])

fromListQ :: [a] -> Queue a
fromListQ ls = Queue ls []

-- inefficient, only used for debugging
toListQ :: Queue a -> [a]
toListQ (Queue front rear) = front ++ (reverse rear)