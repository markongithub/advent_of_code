module Day01 where

import Data.Char (digitToInt, isDigit)

allDigits :: String -> Bool
allDigits str = all isDigit str

sumDigits :: String -> Int
sumDigits str = sum $ map digitToInt str
