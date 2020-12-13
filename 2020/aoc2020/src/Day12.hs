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

data WaypointState = WaypointState Int Int Int Int

manhattan2 :: WaypointState -> Int
manhattan2 (WaypointState x y _ _) = abs x + abs y

waypointNorth :: WaypointState -> Int -> WaypointState
waypointNorth (WaypointState x y wX wY) d = WaypointState x y wX (wY + d)

waypointSouth s d = waypointNorth s (-d)

waypointEast :: WaypointState -> Int -> WaypointState
waypointEast (WaypointState x y wX wY) d = WaypointState x y (wX + d) wY

waypointWest s d = waypointEast s (-d)

waypointRight (WaypointState x y wX wY) theta = let
  realTheta = theta `mod` 360
  (newWX, newWY) = case realTheta of
    0   -> (wX, wY)
    90  -> (wY, -wX)
    180 -> (-wX, -wY)
    270 -> (-wY, wX)
  in WaypointState x y newWX newWY

waypointLeft s theta = waypointRight s (-theta)

moveForward2 :: WaypointState -> Int -> WaypointState
moveForward2 (WaypointState x y wX wY) k = let
  newX = x + (k * wX)
  newY = y + (k * wY)
  in WaypointState newX newY wX wY

runInstruction2 :: WaypointState -> String -> WaypointState
runInstruction2 s str = let
  instructionCode = head str
  parameter = read (tail str) :: Int
  function2 = case instructionCode of
    'N' -> waypointNorth
    'S' -> waypointSouth
    'E' -> waypointEast
    'W' -> waypointWest
    'L' -> waypointLeft
    'R' -> waypointRight
    'F' -> moveForward2
    _   -> error "invalid instruction code"
  in function2 s parameter

runInstructions2 :: WaypointState -> [String] -> WaypointState
runInstructions2 = foldl runInstruction2

initialState2 = WaypointState 0 0 10 1

solvePart2Pure :: [String] -> Int
solvePart2Pure xs = manhattan2 $ runInstructions2 initialState2 xs

solvePart2 :: IO Int
solvePart2 = let
  input = fmap lines $ readFile "data/input12.txt"
  in fmap solvePart2Pure input
