module Day25 where

-- we return a carry digit and a sum digit
addDigits :: Char -> Char -> (Char, Char)
addDigits a1 a2 = let
  alpha1 = min a1 a2
  alpha2 = max a1 a2
  in case (alpha1, alpha2) of
    ('0', x)   -> ('0', x)
    (x, '0')   -> ('0', x)
    ('-', '=') -> ('-', '2')
    ('-', '-') -> ('0', '=')
    ('-', '1') -> ('0', '0')
    ('-', '2') -> ('0', '1')
    ('1', '1') -> ('0', '2')
    ('1', '2') -> ('1', '=')
    ('1', '=') -> ('0', '-')
    ('2', '2') -> ('1', '-')
    ('2', '=') -> ('0', '0')
    ('=', '=') -> ('-', '1')
    _ -> error ("I did not expect " ++ show (alpha1, alpha2))

addSNAFU0 :: (String, String) -> String -> String -> (String, String)
addSNAFU0 ([],_) carries accu = (carries, accu)
addSNAFU0 (x:xs,y:ys) carries accu = let
  (newCarry, newDigit) = addDigits x y
  newAccu = newDigit:accu
  newCarries = newCarry:carries
  in addSNAFU0 (xs, ys) newCarries newAccu

resolveCarries :: String -> String -> String
resolveCarries carries accu = if all (=='0') carries then accu else addSNAFU carries accu

stripLeading0s :: String -> String
stripLeading0s = dropWhile (== '0')

addSNAFU :: String -> String -> String
addSNAFU s1 s2 = let
  (p1, p2) = padToEqualLength s1 s2
  (finalCarries, finalSum) = addSNAFU0 (reverse p1, reverse p2) "0" []
  in stripLeading0s $ resolveCarries finalCarries finalSum

padToEqualLength :: String -> String -> (String, String)
padToEqualLength s1 s2 = let
  difference = length s1 - length s2
  -- if it's negative pad s1
  zeroes = take (abs difference) (repeat '0')
  in if (difference < 0) then (zeroes ++ s1, s2) else (s1, zeroes ++ s2)

testInput = [
    "1=-0-2"
  , "12111"
  , "2=0="
  , "21"
  , "2=01"
  , "111"
  , "20012"
  , "112"
  , "1=-1="
  , "1-12"
  , "12"
  , "1="
  , "122"
  ]

solvePart1Pure :: [String] -> String
solvePart1Pure strs = foldl addSNAFU "0" strs

solvePart1 :: IO String
solvePart1 = fmap (solvePart1Pure . lines) $ readFile "data/input25.txt"