module Main where

import IntCode
import qualified Data.Map as Map

example1 = [3,0,4,0,99]

runP1Example :: [Int] -> Int -> [Int]
runP1Example l input = let
  IntCodeState _ _ _ output = processNextInstruction $ initialStateFromList l [input]
  in output

example2 = [1002,4,3,4,33]
runExample2 :: [Int]
runExample2 = let
  result = processNextInstruction $ initialStateFromList example2 []
  in Map.elems $ memoryMap result

main = do
  putStrLn $ show $ runP1Example example1 9
  putStrLn $ show runExample2
