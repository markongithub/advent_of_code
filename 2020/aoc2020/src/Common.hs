module Common where

-- plagiarizing from my own 2019d03 solution
splitOnSeparator0 :: Char -> String -> [String] -> [String]
splitOnSeparator0 separator str accu
  | null str = reverse accu
  | head str == separator = splitOnSeparator0 separator (tail str) accu
  | otherwise = let
    nextSegment = takeWhile (/= separator) str
    remainder = dropWhile (/= separator) str
    newAccu = nextSegment:accu
    in splitOnSeparator0 separator remainder newAccu

splitOnSeparator :: Char -> String -> [String]
splitOnSeparator separator str = splitOnSeparator0 separator str []

splitOnCommas :: String -> [String]
splitOnCommas = splitOnSeparator ','
