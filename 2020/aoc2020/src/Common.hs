module Common where

-- plagiarizing from my own 2019d03 solution
splitOnCommas0 :: String -> [String] -> [String]
splitOnCommas0 str accu = case str of
  [] -> reverse accu
  (',':xs) -> splitOnCommas0 xs accu
  _ -> let
    nextSegment = takeWhile (/= ',') str
    remainder = dropWhile (/= ',') str
    newAccu = nextSegment:accu
    in splitOnCommas0 remainder newAccu

splitOnCommas :: String -> [String]
splitOnCommas str = splitOnCommas0 str []
