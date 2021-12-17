module Day16 where

import Data.Char(digitToInt)

binStringToBits :: String -> [Int]
binStringToBits str = map digitToInt str

charToBits :: Char -> [Int]
charToBits c = let
  str = case c of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"
    _   -> error "that is not a hex digit"
  in binStringToBits str

hexStringToBits :: String -> [Int]
hexStringToBits str = concat $ map charToBits str

parseLiteral :: Int -> [Int] -> (Packet, [Int])
parseLiteral version body = let
  (valueBits, remainder) = parseLiteral0 body
  in (Literal version (bitsToDec valueBits), remainder)

parseLiteral0 :: [Int] -> ([Int], [Int])
parseLiteral0 body = let
  value = tail $ take 5 body
  remainder = drop 5 body
  (recursiveValue, recursiveRemainder) = parseLiteral0 remainder
  in case head body of
    0 -> (value, remainder)
    1 -> (value ++ recursiveValue, recursiveRemainder)

bitsToDec0 :: [Int] -> Int -> Int
bitsToDec0 [] accu = accu
bitsToDec0 (x:xs) accu = let
  newAccu = x + 2 * accu
  in bitsToDec0 xs newAccu

bitsToDec :: [Int] -> Int
bitsToDec ls = bitsToDec0 ls 0

data Packet = Literal Int Int | Operation Int Int [Packet]
  deriving (Eq, Show)

parsePacket :: [Int] -> [Packet]
parsePacket ls = let
  version = bitsToDec $ take 3 ls
  typeID = bitsToDec $ take 3 $ drop 3 ls
  body = drop 6 ls
  (firstPacket, remainder) = case typeID of
    4 -> parseLiteral version body
    _ -> parseOperation version typeID body
  in firstPacket:(parsePacket remainder)

parseOperation :: Int -> Int -> [Int] -> (Packet, [Int])
parseOperation version operator body = let
  version = take 3 body
  operator = take 3 $ drop 3 body
  lengthTypeID = head $ drop 6 body
  in undefined 
