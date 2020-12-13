module Day12 where

data ShipState = ShipState Int Int Int

moveNorth :: ShipState -> Int -> ShipState
moveNorth (ShipState h x y) d = ShipState h x (y + d)

moveSouth :: ShipState -> Int -> ShipState
moveSouth s d = moveNorth s (-d)

moveEast :: ShipState -> Int -> ShipState
moveEast (ShipState h x y) d = ShipState h (x + d) y

moveWest :: ShipState -> Int -> ShipState
moveWest s d = moveEast s (-d)

turnRight :: ShipState -> Int -> ShipState
turnRight (ShipState h x y) theta = let
  newHeading = (h + theta) `mod` 360
  in ShipState newHeading x y

turnLeft :: ShipState -> Int -> ShipState
turnLeft s theta = turnRight s (-theta)

moveForward :: ShipState -> Int -> ShipState
moveForward (ShipState h x y) d = let
  moveFunc = case h of
    0   -> moveNorth
    90  -> moveEast
    180 -> moveSouth
    270 -> moveWest
    _   -> error "Invalid heading"
  in moveFunc (ShipState h x y) d

runInstruction :: ShipState -> String -> ShipState
runInstruction s str = let
  instructionCode = head str
  parameter = read (tail str) :: Int
  function = case instructionCode of
    'N' -> moveNorth
    'S' -> moveSouth
    'E' -> moveEast
    'W' -> moveWest
    'L' -> turnLeft
    'R' -> turnRight
    'F' -> moveForward
    _   -> error "invalid instruction code"
  in function s parameter

runInstructions :: ShipState -> [String] -> ShipState
runInstructions = foldl runInstruction

initialState = ShipState 90 0 0

manhattan :: ShipState -> Int
manhattan (ShipState _ x y) = abs x + abs y

solvePart1Pure :: [String] -> Int
solvePart1Pure xs = manhattan $ runInstructions initialState xs

solvePart1 :: IO Int
solvePart1 = let
  input = fmap lines $ readFile "data/input12.txt"
  in fmap solvePart1Pure input

