module Day21 where

import Data.Char(isDigit)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Debug.Trace (traceShow)

type MonkeyID = String
data MonkeyBehavior = MonkeySay Int | MonkeyDo Char MonkeyID MonkeyID
                      deriving (Eq, Ord, Show)
type MonkeyMap = Map MonkeyID MonkeyBehavior

parseLine :: String -> (MonkeyID, MonkeyBehavior)
parseLine s = let
  monkeyID = take 4 s
  funcStr = drop 6 s
  intVal = read funcStr :: Int
  func = if isDigit (head funcStr) then MonkeySay intVal else parseFunc funcStr
  in (monkeyID, func)

parseFunc :: String -> MonkeyBehavior
parseFunc s = let
  firstMonkey = take 4 s
  secondMonkey = drop 7 s
  op = s!!5
  in MonkeyDo op firstMonkey secondMonkey

charToOp :: Char -> (Int -> Int -> Int)
charToOp char = case char of
  '+' -> (+)
  '-' -> (-)
  '*' -> (*)
  '/' -> div

getValue :: MonkeyMap -> MonkeyID -> Int
getValue mmap monkey = let
  entry = mmap!monkey
  MonkeyDo opC m1 m2 = entry -- if it's MonkeySay we won't eval this
  op = charToOp opC
  in case entry of
    MonkeySay i -> i
    _ -> op (getValue mmap m1) (getValue mmap m2)

testInput = [
    "root: pppw + sjmn"
  , "dbpl: 5"
  , "cczh: sllz + lgvd"
  , "zczc: 2"
  , "ptdq: humn - dvpt"
  , "dvpt: 3"
  , "lfqf: 4"
  , "humn: 5"
  , "ljgn: 2"
  , "sjmn: drzm * dbpl"
  , "sllz: 4"
  , "pppw: cczh / lfqf"
  , "lgvd: ljgn * ptdq"
  , "drzm: hmdt - zczc"
  , "hmdt: 32"
  ]

solvePart1Pure strs = let
  monkeys = Map.fromList $ map parseLine strs
  in getValue monkeys "root"

solvePart1 = let
  strs = fmap lines $ readFile "data/input21.txt"
  in fmap solvePart1Pure strs

data MonkeyOutput = Easy Int | Hard Char MonkeyOutput MonkeyOutput  | IsEqual MonkeyOutput MonkeyOutput | Humn

reverseOp :: Char -> Int -> Bool -> (Int -> Int)
-- humn + 4 = 150
-- humn + c = x
reverseOp '+' c _ = \x -> x - c
-- c - humn = x
reverseOp '-' c False = \x -> c - x  
-- humn - c = x
reverseOp '-' c True = \x -> x + c
-- c * humn = 10
-- c * humn = x
reverseOp '*' c _ = \x -> x `div` c
-- humn / c = x
reverseOp '/' c True = \x -> x * c
-- c / humn = x
reverseOp '/' c False = \x -> c `div` x

monkeyMath :: Char -> MonkeyOutput -> MonkeyOutput -> MonkeyOutput
monkeyMath opC out1 out2 = case (out1, out2) of
  (Easy i1, Easy i2) -> Easy ((charToOp opC) i1 i2)
  _ -> Hard opC out1 out2

getValue2 :: MonkeyMap -> MonkeyID -> MonkeyOutput
getValue2 mmap monkey = let
  entry = mmap!monkey
  MonkeyDo opC m1 m2 = entry -- if it's MonkeySay we won't eval this
  in case monkey of
    "root" -> IsEqual (getValue2 mmap m1) (getValue2 mmap m2)
    "humn" -> Humn
    _  -> case entry of
      MonkeySay i -> Easy i
      _ -> monkeyMath opC (getValue2 mmap m1) (getValue2 mmap m2)

instance Show MonkeyOutput where
  show (Easy i) = show i
  show (Hard opC o1 o2) = concat ["(", show o1, " ", opC:" ", show o2, ")"]
  show (IsEqual o1 o2) = concat [show o1, " = ", show o2]
  show Humn = "x"

hardToFunction :: MonkeyOutput -> (Int -> Int)
hardToFunction Humn x = x
hardToFunction (Hard opC (Easy i1) (Easy i2)) _ = error "this shouldn't happen"
-- hardToFunction (Hard opC h1 (Easy i2)) x = reverseOp opC i2 True $ hardToFunction h1 x
-- hardToFunction (Hard opC (Easy i1) h2) x = reverseOp opC i1 False $ hardToFunction h2 x
hardToFunction (Hard opC h1 (Easy i2)) x = hardToFunction h1 $ reverseOp opC i2 True  x
hardToFunction (Hard opC (Easy i1) h2) x = hardToFunction h2 $ reverseOp opC i1 False x

solveEquation :: MonkeyOutput -> Int
solveEquation (IsEqual h1 (Easy i)) = let
  func = hardToFunction h1
  in func i

-- humn = 301
-- humn - 3 == 298
test1 = Hard '-' Humn (Easy 3)
-- 2 * test1 == 596
-- reverseOp '*' 2 False $ hardToFunction h2 x
test2 = Hard '*' (Easy 2) test1
-- 4 + test2 == 600
test3 = Hard '+' (Easy 4) test2
-- test3 / 4 == 150
test4 = Hard '/' test3 (Easy 4)

solvePart2Pure :: [String] -> Int
solvePart2Pure strs = let
  monkeys :: MonkeyMap
  monkeys = Map.fromList $ map parseLine strs
  in solveEquation $ getValue2 monkeys "root"

solvePart2 = let
  strs = fmap lines $ readFile "data/input21.txt"
  in fmap solvePart2Pure strs

showInOrder :: MonkeyOutput -> String
showInOrder (Easy i) = show i
showInOrder (Hard '/' (Easy i) (Hard _ _ _)) = error "did not expect variable in denominator"
showInOrder (Hard opC o1 o2) = concat ["(", opC:" ", showInOrder o1, " ", showInOrder o2, ")"]
showInOrder (IsEqual o1 o2) = concat [showInOrder o1, " = ", showInOrder o2]
showInOrder Humn = "x"

applyForward :: Int -> MonkeyOutput -> Int
applyForward _ (Easy i) = i
applyForward input (Hard opC o1 o2) = (charToOp opC) (applyForward input o1) (applyForward input o2)
applyForward input Humn = input
applyForward _ (IsEqual _ _) = error "don't use this for IsEqual"

binarySearch :: Int -> Int -> Int -> MonkeyOutput -> Int
binarySearch desired low high monkeyFunc
  | traceShow (low, high, guess, output, compare output desired, output - desired) False = undefined
  | low == high && applyForward low monkeyFunc == desired = low
  | low == high = error $ concat ["ran out of guesses or something at ", show (low, high, guess)]
  | output == desired = guess
  | output < desired = binarySearch desired low (guess - 1) monkeyFunc
  | output > desired = binarySearch desired (guess + 1) high monkeyFunc
  where
     guess = (low + high) `div` 2
     output = applyForward guess monkeyFunc

parsedInputIO :: IO (MonkeyOutput, Int)
parsedInputIO = do
  strs <- fmap lines $ readFile "data/input21.txt"
  let monkeys = Map.fromList $ map parseLine strs
  let IsEqual h1 (Easy i) = getValue2 monkeys "root"
  return (h1, i)
