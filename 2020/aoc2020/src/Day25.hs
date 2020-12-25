module Day25 where

transform0 :: Int -> Int -> Int -> Int -> Int
transform0 subject _ 0 accu = accu
transform0 subject modulus loopSize accu = let
  next = (accu * subject) `mod` modulus
  in transform0 subject modulus (loopSize - 1) next

transform :: Int -> Int -> Int -> Int
transform subject modulus loopSize = transform0 subject modulus loopSize 1

findLoopSize0 :: Int -> Int -> Int -> Int -> Int -> Int
findLoopSize0 subject modulus desiredRemainder current power = let
  thisRemainder = current `mod` modulus
  recurse = findLoopSize0 subject modulus desiredRemainder (subject * thisRemainder) (power + 1)
  in if thisRemainder == desiredRemainder then power else recurse

findLoopSize :: Int -> Int -> Int -> Int
findLoopSize subject modulus desiredRemainder = findLoopSize0 subject modulus desiredRemainder subject 1

day25Input1 = 3248366
day25Input2 = 4738476

solvePart1 = let
  loopSize1 = findLoopSize 7 20201227 day25Input1
  loopSize2 = findLoopSize 7 20201227 day25Input2
  encryptionKey1 = transform day25Input2 20201227 loopSize1
  encryptionKey2 = transform day25Input1 20201227 loopSize2
  in if encryptionKey1 == encryptionKey2 then encryptionKey1
    else error "keys do not match"
