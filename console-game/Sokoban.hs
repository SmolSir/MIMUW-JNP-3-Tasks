{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.String
import Data.Char
 
type Program = IO ()

main :: Program
main = program

program :: Program
program = sokobanGame


------------------
-- TYPES & DATA --
------------------
type Screen = String
type DrawFun = Int -> Int -> Char
type Picture = DrawFun -> DrawFun

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Int Int deriving Eq
data Event = KeyPress String

data Maze = Maze {
  mazeStartCoord :: Coord,
  mazeFunction   :: (Coord -> Tile)
}

data State = State {
  statePlayer    :: Coord,
  stateDirection :: Direction,
  stateBoxes     :: [Coord],
  stateLevel     :: Int,
  stateScore     :: Int
} deriving Eq

data AppState world = StartScreen | Running world

data WithUndo a = WithUndo a [a]

data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   :: (world -> Screen)
}


----------------------
-- SCREEN FUNCTIONS --
----------------------
-- These are the most common terminal dimensions
screenDimX, screenDimY :: Int
screenDimX = 80
screenDimY = 23

-- Given a Picture, creates a string representation of it. Offsets are used
-- to center the (0, 0) point of the image, as the levels are centered around
-- that point as well. BlankFun is applied for coords past the defaultMazeCoords.
buildScreen :: Picture -> Screen
buildScreen pictureFun =
  let xCenterOffset = -div screenDimX 2 in
  let yCenterOffset = -div screenDimY 2 in
  let xRange = [1 .. screenDimX] in
  let yRange = reverse [1 .. screenDimY] in -- reverse to not flip the image
  let blankFun = \_ _ -> ' ' in
  unlines [
    [pictureFun blankFun (x + xCenterOffset) (y + yCenterOffset) | x <- xRange]
    | y <- yRange
  ]

buildTextScreen :: [String] -> Screen
buildTextScreen content =
  let lineOffsetX = \line -> div (screenDimX - length line) 2 in
  let contentOffsetY = div (screenDimY - length content) 2 in
  let copyObject = \count object -> replicate count object in
  let emptyLines = copyObject contentOffsetY "" in
  let centerLine = \line -> (copyObject (lineOffsetX line) ' ') ++ line in
  unlines (emptyLines ++ (map centerLine content) ++ emptyLines)

-- Translates the given Picture by [x, y] vector
translated :: Int -> Int -> Picture -> Picture
translated translateX translateY picture = 
  \offsetDrawFun x y -> 
    let correctDrawFun = 
          \offsetX offsetY -> 
            offsetDrawFun (offsetX + translateX) (offsetY + translateY) in
    picture correctDrawFun (x - translateX) (y - translateY)


---------------------
-- COORD FUNCTIONS --
---------------------
defaultMazeCoords :: [Coord]
defaultMazeCoords = [ (C x y) | x <- [-10 .. 10], y <- [-10 .. 10] ]

-- Translates the picture from default coordinate to the given coordinate
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) picture = translated (fromIntegral x) (fromIntegral y) picture

-- Returns the given coordinate moved 1 unit in the given direction
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C  x     (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C  x     (y - 1)

-- Returns the given coordinate moved 1 unit in the given series of directions
moveCoords :: [Direction] -> Coord -> Coord
moveCoords directions coord = 
  if null directions then coord
  else moveCoords (tail directions) (adjacentCoord (head directions) coord)


---------------------
-- GRAPH FUNCTIONS --
---------------------
dfs :: Eq a => a -> (a -> [a]) -> [a] -> [a]
dfs vertice neighbours visited = 
  let search = \v acc -> if elem v acc then acc else dfs v neighbours acc in
  foldr search (vertice : visited) (neighbours vertice)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = all isOk (dfs initial neighbours [])

-- More efficient than calling reachable n times for each list elem
findReachable :: Eq a => [a] -> a -> (a -> [a]) -> [a]
findReachable vs initial neighbours =
  let visited = dfs initial neighbours [] in
  filter (\v -> elem v visited) vs


--------------------
-- MAZE FUNCTIONS --
--------------------
-- Checks if the given tile is free to stand on
isEmptyTile :: Tile -> Bool
isEmptyTile Storage = True
isEmptyTile Ground  = True
isEmptyTile _       = False

mazeFindTiles :: Maze -> Tile -> [Coord]
mazeFindTiles (Maze _ fun) tile = 
  [coord | coord <- defaultMazeCoords, fun coord == tile]

initialBoxes :: Maze -> [Coord]
initialBoxes maze@(Maze initial _) =
  findReachable (mazeFindTiles maze Box) initial (neighbours maze)

-- Removes just the initially reachable boxes (the other boxes are not
-- present in the state's boxes list, so they can remain visible)
removeBoxes :: Maze -> Maze
removeBoxes maze@(Maze initial fun) =
  let initialCoords = initialBoxes maze in
  let removeCheck = \coord -> elem coord initialCoords && fun coord == Box in
  let boxlessFun = \coord -> if removeCheck coord then Ground else fun coord in
  Maze initial boxlessFun

addBoxes :: [Coord] -> Maze -> Maze
addBoxes coords maze@(Maze initial boxlessFun) =
  let fun = \coord -> if elem coord coords then Box else boxlessFun coord in
  Maze initial fun
  
replaceBoxes :: [Coord] -> Maze -> Maze
replaceBoxes coords maze = (addBoxes coords . removeBoxes) maze

-- Returns the list of neighbours for the given coordinate or an empty list
-- if the current coordinate is not Ground nor Storage
neighbours :: Maze -> (Coord -> [Coord])
neighbours (Maze _ fun) = 
  \currentCoord ->
    if isEmptyTile (fun currentCoord)
      then map (\dir -> adjacentCoord dir currentCoord) [R, U, L, D]
    else []

isClosed :: Maze -> Bool
isClosed maze@(Maze initial fun) =
  let noBlank = \coord -> not (fun coord == Blank) in
  isEmptyTile (fun initial) &&
  isGraphClosed initial (neighbours maze) noBlank

-- This function cannot detect that a maze is not closed. Use isClosed for that
isSane :: Maze -> Bool
isSane maze@(Maze initial _) =
  let boxlessMaze = removeBoxes maze in
  let boxes = mazeFindTiles maze Box in
  let storages = mazeFindTiles boxlessMaze Storage in
  let reachableBoxCount = 
        length (findReachable boxes initial (neighbours maze)) in
  let reachableStorageCount =
        length (findReachable storages initial (neighbours boxlessMaze)) in
  reachableStorageCount >= reachableBoxCount


---------------------
-- STATE FUNCTIONS --
---------------------
initialLevel :: Int
initialLevel = 0

loadLevelInitialState :: Int -> State
loadLevelInitialState level = 
  let maze@(Maze initial fun) = mazes !! level in
  State {
    statePlayer    = initial,
    stateDirection = D,
    stateBoxes     = initialBoxes maze,
    stateLevel     = level,
    stateScore     = 0
}

previousLevel :: State -> State
previousLevel state =
  let currentLevel = stateLevel state in
  let minLevel = 0 in
  if currentLevel == minLevel
    then state
  else loadLevelInitialState (currentLevel - 1)

nextLevel :: State -> State
nextLevel state =
  let currentLevel = stateLevel state in
  let maxLevel = length mazes - 1 in
  if currentLevel == maxLevel
    then state
  else loadLevelInitialState (currentLevel + 1)

getMaze :: State -> Maze
getMaze state = mazes !! stateLevel state

movePlayer :: State -> Coord -> State
movePlayer state nextCoord = 
  let currentScore = stateScore state in
  state { statePlayer = nextCoord, stateScore = currentScore + 1 }

moveBox :: Coord -> Coord -> Coord -> Coord
moveBox wanted target current = if current == wanted then target else current

moveBoxPlayer :: State -> Coord -> Coord -> State
moveBoxPlayer state nextCoord afterNextCoord =
  let nextBoxes = map (moveBox nextCoord afterNextCoord) (stateBoxes state) in
  let nextState = state { stateBoxes = nextBoxes } in
  movePlayer nextState nextCoord

tryMove :: State -> Direction -> State
tryMove state direction =
  let (Maze _ fun) = replaceBoxes (stateBoxes state) (getMaze state) in
  let directedState = state { stateDirection = direction } in
  let nextCoord = adjacentCoord direction (statePlayer state) in
  let nextCoordTile = fun nextCoord in
  let afterNextCoord = adjacentCoord direction nextCoord in
  let afterNextCoordTile = fun afterNextCoord in
  if isEmptyTile nextCoordTile
    then movePlayer directedState nextCoord
  else if nextCoordTile == Box && isEmptyTile afterNextCoordTile
    then moveBoxPlayer directedState nextCoord afterNextCoord
  else directedState
      
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
  | key == "P"      = previousLevel state
  | key == "N"      = nextLevel state
  | key == "R"      = loadLevelInitialState (stateLevel state)
  | isWinning state = state
  | key == "Right"  = tryMove state R
  | key == "Up"     = tryMove state U
  | key == "Left"   = tryMove state L
  | key == "Down"   = tryMove state D
handleEvent _ state = state

isWinning :: State -> Bool
isWinning state = 
  let (Maze _ fun) = getMaze state in
  let boxStorageCheck = \coord -> fun coord == Storage in
  all boxStorageCheck (stateBoxes state)


-----------------------------
-- ACTIVITY & IO FUNCTIONS --
-----------------------------
resettable :: Activity activity -> Activity activity
resettable (Activity initialWorld handleFunction drawFunction) =
  Activity initialWorld handleFun drawFunction
  where
    handleFun (KeyPress "Esc") _     = initialWorld
    handleFun event            state = handleFunction event state

withStartScreen :: Activity activity -> Activity (AppState activity)
withStartScreen (Activity initialWorld handleFunction drawFunction) =
  Activity initWorld handleFun drawFun
  where
    initWorld = StartScreen
    handleFun (KeyPress " ")   StartScreen     = Running initialWorld
    handleFun _                StartScreen     = StartScreen
    handleFun event            (Running state) = Running (handleFunction event state)
    drawFun StartScreen     = startScreen
    drawFun (Running state) = drawFunction state

withUndo :: Eq activity => Activity activity -> Activity (WithUndo activity)
withUndo (Activity initialState handleFunction drawFunction) =
  Activity initState handleFun drawFun
  where
    initState = WithUndo initialState []
    handleFun (KeyPress key) (WithUndo currentState currentStack)
      | key == "U" = 
          case currentStack of 
            state : stack -> WithUndo state stack
            []            -> WithUndo currentState []
    handleFun event          (WithUndo currentState currentStack)
      | state == currentState = WithUndo currentState currentStack
      | isInitial state       = WithUndo state [] -- clear the stack after R, P, N
      | otherwise             = WithUndo state (currentState : currentStack)
      where 
        state = handleFunction event currentState
        getInitialState = \key -> handleFunction (KeyPress key) currentState
        isInitial state = elem state (map getInitialState ["R", "P", "N"])
    drawFun (WithUndo state _) = drawFunction state

write, writeLine :: String -> IO ()
write = putStr
writeLine = putStrLn

emptyScreen :: IO ()
emptyScreen = write "\ESCc"

initialActivity :: Activity State
initialActivity = Activity (loadLevelInitialState 0) handleEvent draw

runActivity :: Activity activity -> IO ()
runActivity (Activity initialWorld handleFunction drawFunction) =
  do
    hSetBuffering stdin NoBuffering
    emptyScreen
    write (drawFunction initialWorld)
    handleInput "" initialWorld
  where
    handleInput input state =
      do
        currentInput <- getInput
        let (key, keys) = matchInput (input ++ currentInput)
        if key == "Q" -- allow the player to quit the entire game
          then return ()
        else 
          do
            let currentState = handleFunction (KeyPress key) state
            emptyScreen
            write (drawFunction currentState)
            handleInput keys currentState 

getInput :: IO String
getInput = 
  nextKey
  where
    nextKey =
      do
        key <- getChar
        keys <- nextKeys
        return (key : keys)
    nextKeys =
      do
        ready <- hReady stdin
        if ready 
          then nextKey 
        else return []

matchInput ('\ESC' : '[' : 'C' : rest) = ("Right", rest)
matchInput ('\ESC' : '[' : 'A' : rest) = ("Up", rest)
matchInput ('\ESC' : '[' : 'D' : rest) = ("Left", rest)
matchInput ('\ESC' : '[' : 'B' : rest) = ("Down", rest)
matchInput ('\ESC' : rest) = ("Esc", rest)
matchInput (key : rest) = ([toUpper key], rest)
matchInput rest = ([], rest)

sokobanGame :: IO ()
sokobanGame = 
  (runActivity . resettable . withStartScreen . withUndo) initialActivity


--------------------
-- DRAW FUNCTIONS --
--------------------
blank :: Picture
blank = id

(&) :: Picture -> Picture -> Picture
(&) = (.)

pictures :: [Picture] -> Picture
pictures []         = blank
pictures (p : pics) = p & pictures pics

drawChar :: Char -> Picture
drawChar c = \drawFun x y -> if (C x y) == (C 0 0) then c else drawFun x y

drawTile :: Tile -> Picture
drawTile Wall    = drawChar '#'
drawTile Ground  = drawChar ' '
drawTile Storage = drawChar '.'
drawTile Box     = drawChar '$'
drawTile Blank   = drawChar ' '

drawPlayer :: State -> Picture
drawPlayer state = -- if player stands on Storage then '+' else '@'
  let (Maze _ fun) = getMaze state in
  let playerCoord = statePlayer state in
  let c = if fun playerCoord == Storage then '+' else '@' in
  drawChar c

drawCheckmarks :: State -> Picture
drawCheckmarks state = 
  let (Maze _ fun) = getMaze state in
  pictures([atCoord boxCoord (drawChar '*') -- '*' is a Box on a Storage Tile
    | boxCoord <- stateBoxes state, fun boxCoord == Storage ])

drawState :: State -> Screen
drawState state =
  let player = atCoord (statePlayer state) (drawPlayer state) in
  let checkmarks = drawCheckmarks state in
  let (Maze _ fun) = replaceBoxes (stateBoxes state) (getMaze state) in
  let currentMaze = 
        pictures([atCoord coord (drawTile (fun coord))
          | coord <- defaultMazeCoords ]) in
  buildScreen (player & checkmarks & currentMaze)

draw :: State -> Screen
draw state
  | isWinning state = endScreen state
  | otherwise       = drawState state


------------------
-- START SCREEN --
------------------
startScreenContent :: [String]
startScreenContent = [
  "Sokoban!",
  "press Space to begin",
  "",
  "",
  "",
  "move around    - ←↕→",
  "return to menu - Esc",
  "previous maze  -  P ",
  "next maze      -  N ",
  "reset maze     -  R ",
  "undo last move -  U ",
  "exit           -  Q "
  ]

startScreen :: Screen
startScreen = buildTextScreen startScreenContent


----------------
-- END SCREEN --
----------------
endScreenContent :: State -> [String]
endScreenContent state =
  let level = show (stateLevel state + 1) in
  let total = show (length mazes) in
  let moves = show (stateScore state) in
  let levelMessage = "Maze " ++ level ++ "/" ++ total ++ " complete!" in
  let movesMessage = "score: " ++ moves in
  [levelMessage, movesMessage]

endScreen :: State -> Screen
endScreen state = buildTextScreen (endScreenContent state)


-----------
-- MAZES --
-----------
-- This is a shared base of Sokoban mazes created by students attending
-- the 2022/23 MIMUW JNP-3 Haskell course. Any student is free to copy
-- the contents of this document for themselves, as well as to add the
-- mazes they have created themselves. Learn more about the base at
-- https://bit.ly/SokobanMazesMIMUW

-- correct mazes
mazes :: [Maze]
mazes = [
  Maze (C (-2) (-2)) easy_testMaze_GN,
  Maze (C 1 (-1))    easy_spiralMaze_DM,
  Maze (C 0 0)       easy_decoratedMaze_BS,
  Maze (C 1 1)       medium_maze4_GN,
  Maze (C 0 1)       medium_maze3_GN,
  Maze (C 1 (-3))    hard_maze2_GN
  ]

easy_testMaze_GN :: Coord -> Tile
easy_testMaze_GN (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

hard_maze2_GN :: Coord -> Tile
hard_maze2_GN (C x y)
  | abs x > 4  || abs y > 4      = Blank
  | abs x == 4 || abs y == 4     = Wall
  | x ==  2 && y <   0           = Wall
  | x >= -1 && y ==  1 && x <= 2 = Wall
  | x == -3 && y ==  1           = Wall
  | x ==  0 && y ==  3           = Wall
  | x ==  0 && y ==  0           = Wall
  | x ==  3 && y == -3           = Storage
  | x ==  1 && y ==  2           = Storage
  | x == -3 && y ==  2           = Storage
  | x ==  1 && y == -1           = Storage
  | x == -2 && y ==  1           = Box
  | x ==  2 && y ==  2           = Box
  | x <=  1 && y == -2 && x >= 0 = Box
  | otherwise                    = Ground

medium_maze3_GN :: Coord -> Tile
medium_maze3_GN (C x y)
  | abs x >  4 || abs y >  4           = Blank
  | abs x == 4 || abs y == 4           = Wall
  | x ==     1 && y <      0           = Wall
  | x ==    -3 && y ==    -2           = Wall
  | x <=     1 && x >     -2 && y == 0 = Wall
  | x >     -3 && x <      3 && y == 2 = Wall
  | x ==     3 && y >      1           = Storage
  | y ==    -2 && x <      0           = Box
  | y ==    -2 && x ==     2           = Box
  | y ==    0  && x ==     3           = Box
  | y == -1    && x > 1      && x < 4  = Storage
  | otherwise                          = Ground

medium_maze4_GN :: Coord -> Tile
medium_maze4_GN (C x y)
  | abs x > 4  || abs y > 4                  = Blank
  | abs x == 4 || abs y == 4 || x == -3      = Wall
  | x == -2 && (y == 3 || y == 0)            = Wall
  | x == -1 &&  y == -1                      = Wall
  | x == -0 &&  y == 1                       = Wall
  | x ==  3 &&  y == 0                       = Wall
  | x <   0 && (y == 2 || y == -3)           = Storage
  | x == -1 &&  y == 1                       = Storage
  | x ==  0 && (y == 2 || y == 0 || y == -1) = Box
  | x ==  1 &&  y == -2                      = Box
  | x ==  2 &&  y == -3                      = Box
  | otherwise                                = Ground

easy_spiralMaze_DM :: Coord -> Tile
easy_spiralMaze_DM (C x y)
  | abs x >  4 || abs y > 4      = Blank
  | abs x == 4                   = Wall
  | abs y == 4                   = Wall
  | x ==  2 && y <=  1           = Wall
  | x >= -1 && x <=  2 && y == 1 = Wall
  | x == -1 && y >= -1 && y <= 1 = Wall
  | x ==  0 && y == -1           = Box
  | x ==  3 && y == -3           = Storage
  | otherwise                    = Ground

easy_decoratedMaze_BS :: Coord -> Tile
easy_decoratedMaze_BS (C x y)
  | abs x > 6  || abs y > 4          = Blank
  | abs x == 6 || abs y == 4         = Wall
  | abs x + abs y > 6                = Box
  | abs x == 4 || abs y == 2         = Wall
  | elem (x, y) [(-3, 1), (3, -1)]   = Box
  | x == 0     && abs y == 1         = Box
  | elem (x, y) [(-3, -1), (3, 1)]   = Storage
  | elem (x, y) [(-2, 1), (2, -1)]   = Wall
  | abs x > 1 && abs x < 4 && y == 0 = Wall
  | otherwise                        = Ground

-- bad mazes
badMazes :: [Maze]
badMazes = [
  Maze (C (-2) (-2)) badTestMaze_BS,
  Maze (C 1 (-1))    cutOffStorageMaze_DM,
  Maze (C (-1) 0)    holeInTheWallMaze_BS
  ]

badTestMaze_BS :: Coord -> Tile
badTestMaze_BS (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2                  = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

cutOffStorageMaze_DM :: Coord -> Tile
cutOffStorageMaze_DM (C x y)
  | abs x > 7 || abs y > 7                            = Blank
  | abs x == 7                                        = Wall
  | abs y == 7                                        = Wall
  | x >= 4 && y == 4                                  = Wall
  | x == 4 && y >= 4                                  = Wall
  | x >= 5 && y >= 5                                  = Storage
  | elem (x, y) [(-6, 6), (-6, -6), (6, -6), (6, -5)] = Storage
  | x == 0 && elem y [-4 .. 2]                        = Box
  | otherwise                                         = Ground

holeInTheWallMaze_BS :: Coord -> Tile
holeInTheWallMaze_BS (C x y)
  | abs x > 2 || abs y > 1   = Blank
  | x == -2 && y == 0        = Ground
  | abs x == 2 || abs y == 1 = Wall
  | x == 0 && y == 0         = Box
  | x == 1 && y == 0         = Storage
  | otherwise                = Ground
