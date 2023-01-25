module Day13 where

import Data.Char (isDigit)
data Packet = IntData Int | ListData [Packet]
              deriving (Eq, Ord, Show)

consPackets :: Packet -> Packet -> Packet
consPackets _ (IntData _) = error "what the fuck am I doing"
consPackets x (ListData xs) = ListData (x:xs)

consParse :: Packet -> ([Packet], String) -> ([Packet], String)
consParse x (xs, s) = ((x:xs), s)
-- parsePacket "[[1],2]"
--  (thisList, otherCrap) = parsePacket "[1],2]"
--    (thisList, otherCrap) = parsePacket "1],2]"
--      (thisInt, remainder) = parseInt "1],2]"
--      stringOfDigits = "1"
--      value = 1
--      remainder = "],2]"
parsePacket :: String -> ([Packet], String)
parsePacket [] = ([], [])
parsePacket (x:xs)
  | x == ']' = ([], xs)
  | x == ',' = parsePacket xs
  | isDigit x = let
      (thisInt, remainder) = parseInt (x:xs)
      in consParse (IntData thisInt) (parsePacket remainder)
  | x == '[' = let
      (thisList, otherCrap) = parsePacket xs
      in (thisList, otherCrap)

parseInt :: String -> (Int, String)
parseInt (x:xs) = let
  otherDigits = takeWhile isDigit xs
  stringOfDigits = (x:otherDigits)
  value = (read stringOfDigits :: Int)
  remainder = drop (length stringOfDigits - 1) xs
  in (value, remainder)
--  restOfThisList :: [Packet]
--  (restOfThisList, lastRemainder) = parsePacket remainder
--  thisList = (IntData value):restOfThisList
--  in (thisList, lastRemainder)