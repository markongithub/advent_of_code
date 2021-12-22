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

parsePackets :: [Int] -> Int -> ([Packet], [Int])
parsePackets [] _ = ([], [])
parsePackets ls 0 = ([], ls)
parsePackets ls count = let
  version = bitsToDec $ take 3 ls
  typeID = bitsToDec $ take 3 $ drop 3 ls
  body = drop 6 ls
  (firstPacket, remainder) = case typeID of
    4 -> parseLiteral version body
    _ -> parseOperation version typeID body
  (otherPackets, finalRemainder) = parsePackets remainder (count-1)
  in (firstPacket:otherPackets, finalRemainder)

parseBitString :: String -> Packet
parseBitString str = let
  bits = binStringToBits str
  in head $ fst $ parsePackets bits 1

parseHex :: String -> Packet
parseHex str = let
  bits = hexStringToBits str
  in head $ fst $ parsePackets bits 1

parseOperation :: Int -> Int -> [Int] -> (Packet, [Int])
parseOperation version operator body = let
  lengthTypeID = head body
  countSubpackets = lengthTypeID == 1
  func = if countSubpackets then parseSubpacketsByCount else parseSubpacketsByBitLength
  (subpackets, remainder) = func $ tail body
  in (Operation version operator subpackets, remainder)

parseSubpacketsByCount :: [Int] -> ([Packet], [Int])
parseSubpacketsByCount body = let
  count = bitsToDec $ take 11 body
  in parsePackets (drop 11 body) count

parseSubpacketsByBitLength :: [Int] -> ([Packet], [Int])
parseSubpacketsByBitLength body = let
  bitLength = bitsToDec $ take 15 body
  subPacketsBits = take bitLength $ (drop 15 body)
  remainder = drop (bitLength + 15) body
  (subpackets, innerRemainder) = parsePackets subPacketsBits 99
  in (subpackets, innerRemainder ++ remainder)

versionSum :: Packet -> Int
versionSum packet = case packet of
  Literal v _ -> v
  Operation v _ ps -> v + (sum $ map versionSum ps)

part1Hex :: String -> Int
part1Hex = versionSum . parseHex

puzzleInput = "A059141803C0008447E897180401F82F1E60D80021D11A3DC3F300470015786935BED80A5DB5002F69B4298A60FE73BE41968F48080328D00427BCD339CC7F431253838CCEFF4A943803D251B924EC283F16D400C9CDB3180213D2D542EC01092D77381A98DA89801D241705C80180960E93469801400F0A6CEA7617318732B08C67DA48C27551C00F972830052800B08550A277416401A5C913D0043D2CD125AC4B1DB50E0802059552912E9676931530046C0141007E3D4698E20008744D89509677DBF5759F38CDC594401093FC67BACDCE66B3C87380553E7127B88ECACAD96D98F8AC9E570C015C00B8E4E33AD33632938CEB4CD8C67890C01083B800E5CBDAB2BDDF65814C01299D7E34842E85801224D52DF9824D52DF981C4630047401400042E144698B2200C4328731CA6F9CBCA5FBB798021259B7B3BBC912803879CD67F6F5F78BB9CD6A77D42F1223005B8037600042E25C158FE0008747E8F50B276116C9A2730046801F29BC854A6BF4C65F64EB58DF77C018009D640086C318870A0C01D88105A0B9803310E2045C8CF3F4E7D7880484D0040001098B51DA0980021F17A3047899585004E79CE4ABD503005E610271ED4018899234B64F64588C0129EEDFD2EFBA75E0084CC659AF3457317069A509B97FB3531003254D080557A00CC8401F8791DA13080391EA39C739EFEE5394920C01098C735D51B004A7A92F6A0953D497B504F200F2BC01792FE9D64BFA739584774847CE26006A801AC05DE180184053E280104049D10111CA006300E962005A801E2007B80182007200792E00420051E400EF980192DC8471E259245100967FF7E6F2CF25DBFA8593108D342939595454802D79550C0068A72F0DC52A7D68003E99C863D5BC7A411EA37C229A86EBBC0CB802B331FDBED13BAB92080310265296AFA1EDE8AA64A0C02C9D49966195609C0594223005B80152977996D69EE7BD9CE4C1803978A7392ACE71DA448914C527FFE140"

solvePart1 = part1Hex puzzleInput

intValue :: Packet -> Int
intValue (Literal _ i) = i
intValue (Operation _ o ps) = let
  subvalues = map intValue ps
  in case o of
    0 -> sum subvalues
    1 -> product subvalues
    2 -> minimum subvalues
    3 -> maximum subvalues
    5 -> if subvalues!!0 > subvalues!!1 then 1 else 0
    6 -> if subvalues!!0 < subvalues!!1 then 1 else 0
    7 -> if subvalues!!0 == subvalues!!1 then 1 else 0
    _ -> error "unexpected operator code"

part2Hex :: String -> Int
part2Hex = intValue . parseHex

solvePart2 = part2Hex puzzleInput
