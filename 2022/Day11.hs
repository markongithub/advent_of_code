module Day11 where

import Common (splitAtSep)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map

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

data Monkey = Monkey {
    getItems :: Queue Int
  , getOperation :: Int -> Int
  , getDivisor :: Int
  , getTrueMonkey :: Int
  , getFalseMonkey :: Int
  , getItemsCounted :: Int
}

instance Show Monkey where
  show (Monkey a _ b c d e) = show (toListQ a, b, c, d, e)

parseStartingItems :: String -> Queue Int
parseStartingItems itemsStr = let
  justItemsStr = drop (length "  Starting items: ") itemsStr
  itemStrs = splitAtSep justItemsStr ','
  itemInts = map (\s -> read s :: Int) itemStrs
  in fromListQ itemInts

parseOperation :: String -> (Int -> Int)
parseOperation operationStr = let
  operatorIndex = length "  Operation: new = old "
  operator = case operationStr!!operatorIndex of
    '+' -> (+)
    '*' -> (*)
  operandStr = drop (operatorIndex + 2) operationStr
  number = read (drop (operatorIndex + 2) operationStr) :: Int
  in if operandStr == "old" then (\x -> operator x x) else (operator number)

parseMonkey :: [String] -> (Int, Monkey)
parseMonkey [idStr, itemsStr, operationStr, divisorStr, trueStr, falseStr] = let
  monkeyID = digitToInt (idStr!!7)
  items = parseStartingItems itemsStr
  operation = parseOperation operationStr
  divisor = (read $ drop (length "  Test: divisible by ") divisorStr) :: Int
  trueMonkey = (read (drop (length "    If true: throw to monkey ") trueStr) :: Int)
  falseMonkey = (read (drop (length "    If false: throw to monkey ") falseStr) :: Int)
  in (monkeyID, Monkey items operation divisor trueMonkey falseMonkey 0)

type MonkeyMap = Map Int Monkey

parseMonkeys :: [String] -> [(Int, Monkey)]
parseMonkeys [] = []
parseMonkeys ls = let
  firstMonkey = parseMonkey $ take 6 ls
  remainder = drop 7 ls
  in firstMonkey:(parseMonkeys remainder)

parseMonkeyMap :: [String] -> MonkeyMap
parseMonkeyMap = Map.fromList . parseMonkeys

giveMonkeyItem :: Monkey -> Int -> Monkey
giveMonkeyItem monkey item = let
  oldItems = getItems monkey
  newItems = pushQ oldItems item
  in monkey {getItems = newItems}

inspectItems :: (Int -> Int) -> MonkeyMap ->  Int -> MonkeyMap
inspectItems reduceFunc mmap monkeyID = let
  Monkey items operation divisor trueM falseM itemsCounted = mmap!monkeyID
  (nextItem, remainingItems) = popQ $ items
  newSrcMonkey = Monkey remainingItems operation divisor trueM falseM (itemsCounted + 1)
  worryLevel = operation nextItem
  worryLevel2 = reduceFunc worryLevel -- in part 1 this is "divide by 3"
  destMonkeyID = if (worryLevel2 `mod` divisor == 0) then trueM else falseM
  destMonkey = mmap!destMonkeyID
  newDestMonkey = giveMonkeyItem destMonkey worryLevel2
  newMap1 = Map.insert monkeyID newSrcMonkey mmap
  newMap2 = Map.insert destMonkeyID newDestMonkey newMap1
  in if emptyQ items
       then mmap -- we're done with this monkey
       else inspectItems reduceFunc newMap2 monkeyID -- keep going through the queue

runNRounds :: Int -> (Int -> Int) -> MonkeyMap -> MonkeyMap
runNRounds 0 _ m = m
runNRounds n reduceFunc m = let
  monkeyIDs = Map.keys m
  nextStep = foldl (inspectItems reduceFunc) m monkeyIDs
  in runNRounds (n - 1) reduceFunc nextStep

monkeyBusiness :: MonkeyMap -> Int
monkeyBusiness m = let
  sortedCounts = sort $ map getItemsCounted $ Map.elems m
  [x, y] = take 2 $ reverse sortedCounts
  in x * y

solvePart1Pure :: [String] -> Int
solvePart1Pure strings = let
  divideBy3 x = x `div` 3
  in monkeyBusiness $ runNRounds 20 divideBy3 $ parseMonkeyMap strings

assertEqual :: Eq a => a -> a -> (a, a, Bool)
assertEqual expected actual = (expected, actual, expected == actual)

testPart1 = assertEqual 10605 $ solvePart1Pure testInput

solvePart1 = do
  text <- readFile "data/input11.txt"
  return $ (solvePart1Pure . lines) text

solvePart2Pure :: [String] -> Int
solvePart2Pure strings = let
  mmap = parseMonkeyMap strings
  divisor =  product $ map getDivisor $ Map.elems mmap
  reduce x = x `mod` divisor
  in monkeyBusiness $ runNRounds 10000 reduce $ parseMonkeyMap strings

testPart2 = assertEqual 2713310158 $ solvePart2Pure testInput

solvePart2 = do
  text <- readFile "data/input11.txt"
  return $ (solvePart2Pure . lines) text

testInput = [
    "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 1:"
  , "  Starting items: 54, 65, 75, 74"
  , "  Operation: new = old + 6"
  , "  Test: divisible by 19"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 0"
  , ""
  , "Monkey 2:"
  , "  Starting items: 79, 60, 97"
  , "  Operation: new = old * old"
  , "  Test: divisible by 13"
  , "    If true: throw to monkey 1"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 3:"
  , "  Starting items: 74"
  , "  Operation: new = old + 3"
  , "  Test: divisible by 17"
  , "    If true: throw to monkey 0"
  , "    If false: throw to monkey 1"
  ]