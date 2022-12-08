{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.String
 
type Program = IO ()

main :: Program
main = program

program :: Program
program = sokobanGame

----------
-- DATA --
----------
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Int Int deriving Eq

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
  actDraw   :: (world -> Picture)
}


---------------------
-- COORD FUNCTIONS --
---------------------
defaultMazeCoords :: [Coord]
defaultMazeCoords = [ (C x y) | x <- [-10 .. 10], y <- [-10 .. 10] ]

-- translates the picture from default coordinate to the given coordinate
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) picture = translated (fromIntegral x) (fromIntegral y) picture

-- returns the given coordinate moved 1 unit in the given direction
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C  x     (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C  x     (y - 1)

-- returns the given coordinate moved 1 unit in the given series of directions
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

-- more efficient than calling reachable n times for each list elem
findReachable :: Eq a => [a] -> a -> (a -> [a]) -> [a]
findReachable vs initial neighbours =
  let visited = dfs initial neighbours [] in
  filter (\v -> elem v visited) vs


--------------------
-- MAZE FUNCTIONS --
--------------------
-- checks if the given tile is free to stand on
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

-- removes just the initially reachable boxes (the other boxes are not
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

-- returns the list of neighbours for the given coordinate or an empty list
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

-- This function cannot detect that a maze is not closed. Use isClosed for that.
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


------------------------
-- ACTIVITY FUNCTIONS --
------------------------
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

initialActivity :: Activity State
initialActivity = Activity (loadLevelInitialState 0) handleEvent draw

runActivity :: Activity activity -> IO ()
runActivity (Activity initialWorld handleFunction drawFunction) = 
  activityOf initialWorld handleFunction drawFunction

-- note that the order of functions here is VERY important so that we cannot
-- get a level and not a start screen after reset etc.
sokobanGame :: IO ()
sokobanGame = 
  (runActivity . resettable . withStartScreen . withUndo) initialActivity


--------------------
-- DRAW FUNCTIONS --
--------------------
drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

drawPlayer :: Direction -> Picture
drawPlayer R = playerR
drawPlayer U = playerU
drawPlayer L = playerL
drawPlayer D = playerD

drawCheckmarks :: State -> Picture
drawCheckmarks state = 
  let (Maze _ fun) = getMaze state in
  pictures([atCoord boxCoord check
    | boxCoord <- stateBoxes state, fun boxCoord == Storage ])

drawState :: State -> Picture
drawState state =
  let player = atCoord (statePlayer state) (drawPlayer (stateDirection state)) in
  let checkmarks = drawCheckmarks state in
  let (Maze _ fun) = replaceBoxes (stateBoxes state) (getMaze state) in
  let currentMaze = 
        pictures([atCoord coord (drawTile (fun coord))
          | coord <- defaultMazeCoords ]) in
  player & checkmarks & currentMaze

draw :: State -> Picture
draw state
  | isWinning state = endScreen state & drawState state
  | otherwise       = drawState state


------------------------------
-- DRAWING HELPER FUNCTIONS --
------------------------------
-- creates a colour by scaling the numbers down from <0 .. 255> to <0.0 .. 0.1>
fromRGB :: Double -> Double -> Double -> Colour
fromRGB r g b = let scale = 256.0 in RGB (r / scale) (g / scale) (b / scale)

-- draws a pattern of dots at given points, of given radius and colour
drawDots :: [Point] -> Double -> Colour -> Picture
drawDots points radius colour = 
  let dot = coloured colour (solidCircle radius) in 
  pictures([translated x y dot | (x, y) <- points ])

-- draws a solid square
solidSquare :: Double -> Picture
solidSquare size = solidRectangle size size

-- scales the image by the same factor on both axes
squareScaled :: Double -> Picture -> Picture
squareScaled scale picture = scaled scale scale picture

-- draws a coloured rectangle with rounded edges of given width, height,
-- radius and colour (if width < 2 * radius then we draw 
-- width = 2 * radius, the same for height)
roundedRectangle :: Double -> Double -> Double -> Colour -> Picture
roundedRectangle width height radius colour =
  let safeWidth = max width (2.0 * radius) in
  let safeHeight = max height (2.0 * radius) in
  let offsetX = safeWidth / 2.0 - radius in
  let offsetY = safeHeight / 2.0 - radius in
  let edge = coloured colour (solidCircle radius) in
  let edges = pictures[translated x y edge
        | x <- [-offsetX, offsetX], y <- [-offsetY, offsetY] ] in
  let middle = coloured colour (solidRectangle (2.0 * offsetX) (2.0 * offsetY)) in
  let middleX = coloured colour (solidRectangle safeWidth (2.0 * offsetY)) in
  let middleY = coloured colour (solidRectangle (2.0 * offsetX) safeHeight) in
  edges & middleX & middleY

-- draws a coloured square with rounded edges of given size, radius and 
-- colour (if size < 2 * radius returns a circle with that radius)
roundedSquare :: Double -> Double -> Colour -> Picture
roundedSquare size radius colour = roundedRectangle size size radius colour

-- draws series of lines from the lineList that are scaled by the given 
-- scale, offset vertically by the offsetY, spaced between each other by
-- the spacingY and drawn with the given font & style
startLines :: Double -> Double -> Double -> Font -> TextStyle -> [String] -> Picture
startLines scale offsetY spacingY font style lineList =
  let totalOffsetY = \lineID -> offsetY + fromIntegral lineID * spacingY in
  let linePicture = \line -> styledLettering style font (fromString line) in
  pictures[
    translated 0.0 (totalOffsetY lineID)
      (squareScaled scale (linePicture (lineList !! lineID)))
        | lineID <- [0 .. length lineList - 1] ]


------------------
-- START SCREEN --
------------------
-- font & style
startLogoTextFont, startGameTextFont, startInfoTextFont :: Font
startLogoTextFont = Monospace
startGameTextFont = Monospace
startInfoTextFont = Monospace

startLogoTextStyle, startGameTextStyle, startInfoTextStyle :: TextStyle
startLogoTextStyle = Bold
startGameTextStyle = Bold
startInfoTextStyle = Plain

-- colours
startBackgroundColour :: Colour
startBackgroundColour = mixed [translucent white, white]

-- dimensions
startBackgroundSize :: Double
startBackgroundSize = 20.0

startLogoScale, startGameScale, startInfoScale :: Double
startLogoScale = 3.0
startGameScale = 0.8125
startInfoScale = 0.625

startLogoOffsetY, startGameOffsetY, startInfoOffsetY :: Double
startLogoOffsetY = 5.0
startGameOffsetY = 3.0
startInfoOffsetY = -5.0

startBackgroundOffsetY :: Double
startBackgroundOffsetY = (startGameOffsetY + startInfoOffsetY) / 2.0

startGameSpacingY, startInfoSpacingY :: Double
startGameSpacingY = -0.9375
startInfoSpacingY = -0.75 -- this value is negative

-- drawing segments
startInfoLines :: [String]
startInfoLines = [
  "move around    - ←↕→",
  "previous maze  -  P ",
  "next maze      -  N ",
  "reset maze     -  R ",
  "undo last move -  U ",
  "exit           - Esc"
  ]

startGameLines :: [String]
startGameLines = [
  "press Space to begin"
  ]

startBackgroundMapBools :: [Bool]
startBackgroundMapBools = 
  map (\maze -> isClosed maze && isSane maze) (mazes ++ badMazes)

startBackgroundDrawBools :: [Bool] -> Picture
startBackgroundDrawBools bools =
  let boolsOffsetX = (1.0 - fromIntegral columns) / 2.0 in
  let boolsOffsetY = (fromIntegral rows - 1.0) / 2.0 in
  translated boolsOffsetX boolsOffsetY (drawBool 0 bools)
  where maxColumns = 15
        columns = min (length bools) maxColumns
        rows = ceiling (fromIntegral (length bools) / fromIntegral columns)
        
        drawBool _ [] = blank
        drawBool idx (currentBool : otherBools) =
          translated ( fromIntegral (mod idx columns))
                     (-fromIntegral (div idx columns))
                     (pictureOfBool currentBool)
          & drawBool (idx + 1) otherBools

        pictureOfBool True  = coloured (dark green) (solidCircle 0.4)
        pictureOfBool False = coloured (dark red)   (solidCircle 0.4)


startBackground :: Picture
startBackground = translated 0.0 startBackgroundOffsetY 
                    (startBackgroundDrawBools startBackgroundMapBools)

startInfo :: Picture
startInfo = startLines startInfoScale startInfoOffsetY startInfoSpacingY 
                       startInfoTextFont startInfoTextStyle startInfoLines

startGame :: Picture
startGame = startLines startGameScale startGameOffsetY startGameSpacingY
                       startGameTextFont startGameTextStyle startGameLines

startLogo :: Picture
startLogo = translated 0.0 startLogoOffsetY
              (squareScaled startLogoScale
                (styledLettering startLogoTextStyle startLogoTextFont "Sokoban!"))

-- final assembly
startScreen :: Picture
startScreen = startLogo & startGame & startInfo & startBackground


----------------
-- END SCREEN --
----------------
-- font & style
endTextFont :: Font
endTextFont = Monospace

endTextStyle :: TextStyle
endTextStyle = Plain

-- colours
endBackgroundColour :: Colour
endBackgroundColour = mixed [translucent white, white]

-- dimensions
endBackgroundSize :: Double
endBackgroundSize = 20.0

endSummaryLevelScale, endSummaryMovesScale :: Double
endSummaryLevelScale = 1.25
endSummaryMovesScale = 0.875

endSummaryLevelOffsetY, endSummaryMovesOffsetY :: Double
endSummaryLevelOffsetY = 0.5
endSummaryMovesOffsetY = -0.75

-- drawing segments
endBackground :: Picture
endBackground = coloured endBackgroundColour (solidSquare endBackgroundSize)

summary :: State -> Picture
summary state = 
  let level = show (stateLevel state + 1) in -- +1 because first index is 0
  let total = show (length mazes) in
  let moves = show (stateScore state) in
  let levelMessage = "Maze " ++ level ++ "/" ++ total ++ " complete!" in
  let movesMessage = "score: " ++ moves in
  translated 0.0 endSummaryLevelOffsetY
    (squareScaled endSummaryLevelScale
      (styledLettering endTextStyle endTextFont (fromString levelMessage))) &
  translated 0.0 endSummaryMovesOffsetY
    (squareScaled endSummaryMovesScale
      (styledLettering endTextStyle endTextFont (fromString movesMessage)))

-- final assembly
endScreen :: State -> Picture
endScreen state = summary state & endBackground

--scaled 3 3 (lettering "You win!")


----------
-- WALL --
----------
-- colours
wallOutlineColour, wallFrameColour, wallCenterColour, wallTrussColour :: Colour
wallOutlineColour = fromRGB 101 115 126 -- darker steel
wallFrameColour = fromRGB 123 139 150 -- steel
wallCenterColour = fromRGB 145 163 174 -- lighter steel
wallTrussColour = translucent wallFrameColour

-- dimensions
wallOutlineSize, wallOutlineEdgeRadius :: Double
wallOutlineSize = 1.0
wallOutlineEdgeRadius = 0.0

wallFrameSize, wallFrameEdgeRadius :: Double
wallFrameSize = 0.925
wallFrameEdgeRadius = 0.05

wallCenterSize, wallCenterEdgeRadius :: Double
wallCenterSize = 0.7
wallCenterEdgeRadius = 0.05

wallTrussSize, wallTrussEdgeRadius :: Double
wallTrussSize = (wallFrameSize + wallCenterSize) * sqrt(2.0) / 2.0
wallTrussEdgeRadius = 0.025

-- drawing segments
wallOutline, wallFrame, wallCenter, wallSingleTruss, wallTruss :: Picture
wallOutline = roundedSquare wallOutlineSize wallOutlineEdgeRadius wallOutlineColour
wallFrame = roundedSquare wallFrameSize wallFrameEdgeRadius wallFrameColour
wallCenter = roundedSquare wallCenterSize wallCenterEdgeRadius wallCenterColour
wallSingleTruss = roundedRectangle wallTrussSize 0 wallTrussEdgeRadius wallTrussColour
wallTruss = pictures[rotated angle wallSingleTruss | angle <- [-pi / 4, pi / 4] ]

-- final assembly
wall :: Picture
wall = wallTruss & wallCenter & wallFrame & wallOutline


------------
-- GROUND --
------------
-- colours
groundDotColour, groundBackColour :: Colour
groundDotColour = lighter 0.075 (fromRGB 145 163 174) -- light steel
groundBackColour = lighter 0.125 (fromRGB 145 163 174) -- lighter steel

-- dimensions
groundSmallDotRadius, groundLargeDotRadius :: Double
groundSmallDotRadius = 0.025
groundLargeDotRadius = 0.05

-- dots positioning - should be the same for all players!
groundSmallDotsPoints, groundLargeDotsPoints :: [Point]
groundSmallDotsPoints = 
  [(-0.3, 0.4), (0.0, -0.2), (-0.4, -0.1), (0.2, -0.1), (0.1, 0.1)]
groundLargeDotsPoints = 
  [(0.0, 0.4), (-0.2, 0.0), (-0.3, -0.4), (0.3, -0.4), (0.4, 0.3)]

-- drawing segments
groundBack, groundSmallDots, groundLargeDots :: Picture
groundBack = coloured groundBackColour (solidRectangle 1 1)
groundSmallDots = drawDots groundSmallDotsPoints groundSmallDotRadius groundDotColour
groundLargeDots = drawDots groundLargeDotsPoints groundLargeDotRadius groundDotColour

-- final assembly
ground :: Picture
ground = groundSmallDots & groundLargeDots & groundBack


-------------
-- STORAGE --
-------------
-- colours
storageCenterColour, storageBorderColour :: Colour
storageCenterColour = translucent (light red)
storageBorderColour = translucent (lighter 0.1 red)

-- dimensions
storageCenterRadius, storageBorderRadius :: Double
storageCenterRadius = 0.15
storageBorderRadius = 0.2

-- drawing segments
storageCenter, storageBorder :: Picture
storageCenter = coloured storageCenterColour (solidCircle storageCenterRadius)
storageBorder = coloured storageBorderColour (solidCircle storageBorderRadius)

-- final assembly
storage :: Picture
storage = storageCenter & storageBorder & ground


---------
-- BOX --
---------
-- colours
boxOutlineColour, boxFrameColour, boxCenterColour, boxTrussColour :: Colour
boxOutlineColour = dark brown
boxFrameColour = brown
boxCenterColour = light brown
boxTrussColour = darker 0.0125 brown

-- dimensions
boxOutlineSize, boxOutlineEdgeRadius :: Double
boxOutlineSize = 0.9
boxOutlineEdgeRadius = boxFrameEdgeRadius + (boxOutlineSize - boxFrameSize) / 2.0

boxFrameSize, boxFrameEdgeRadius :: Double
boxFrameSize = 0.825
boxFrameEdgeRadius = 0.05

boxCenterSize, boxCenterEdgeRadius :: Double
boxCenterSize = 0.6
boxCenterEdgeRadius = 0.0

boxTrussSize, boxTrussEdgeRadius, boxTrussMargin :: Double
boxTrussSize = boxCenterSize * sqrt(2.0)
boxTrussEdgeRadius = 0.075
boxTrussMargin = boxCenterSize - 0.0625

-- drawing segments
boxOutline, boxFrame, boxCenter, boxSingleTruss, boxTruss :: Picture
boxOutline = roundedSquare boxOutlineSize boxOutlineEdgeRadius boxOutlineColour
boxFrame = roundedSquare boxFrameSize boxFrameEdgeRadius boxFrameColour
boxCenter = roundedSquare boxCenterSize boxCenterEdgeRadius boxCenterColour
boxSingleTruss = roundedRectangle boxTrussSize 0 boxTrussEdgeRadius boxTrussColour
boxTruss = clipped boxTrussMargin boxTrussMargin (rotated (pi / 4) boxSingleTruss)

-- final assembly
box :: Picture
box = boxTruss & boxCenter & boxFrame & boxOutline & ground


----------------
-- CHECK MARK --
----------------
-- colours
checkArmColour, checkCenterColour, checkOutlineColour :: Colour
checkArmColour = white
checkCenterColour = fromRGB 22 198 12
checkOutlineColour = fromRGB 19 168 10

-- dimensions
checkShortArmSize, checkLongArmSize, checkArmEdgeRadius, checkArmAngle :: Double
checkShortArmSize = 0.15
checkLongArmSize = checkShortArmSize * 2.0
checkArmEdgeRadius = 0.025
checkArmAngle = -pi / 4

checkShortArmOffsetX, checkLongArmOffsetY, checkArmsOffsetX, checkArmsOffsetY :: Double
checkShortArmOffsetX = -(checkShortArmSize / 2.0 - checkArmEdgeRadius)
checkLongArmOffsetY = checkLongArmSize / 2.0 - checkArmEdgeRadius
checkArmsOffsetX = -0.05
checkArmsOffsetY = -0.1

checkCenterSize, checkCenterEdgeRadius :: Double
checkCenterSize = 0.4
checkCenterEdgeRadius = 0.05

checkOutlineSize, checkOutlineEdgeRadius :: Double
checkOutlineSize = checkCenterSize + 2.0 * checkCenterEdgeRadius
checkOutlineEdgeRadius = 2.0 * checkCenterEdgeRadius

checkOffsetX, checkOffsetY, checkScale :: Double
checkOffsetX = 0.175
checkOffsetY = -0.175
checkScale = 0.75

-- drawing segments
checkShortArm, checkLongArm, checkArms :: Picture
checkShortArm = translated checkShortArmOffsetX 0
  (roundedRectangle checkShortArmSize 0 checkArmEdgeRadius checkArmColour)
checkLongArm = translated 0 checkLongArmOffsetY
  (roundedRectangle 0 checkLongArmSize checkArmEdgeRadius checkArmColour)
checkArms = translated checkArmsOffsetX checkArmsOffsetY (rotated checkArmAngle 
  (checkShortArm & checkLongArm))
  
checkCenter, checkOutline, checkBackground :: Picture
checkCenter = roundedSquare checkCenterSize checkCenterEdgeRadius checkCenterColour
checkOutline = roundedSquare checkOutlineSize checkOutlineEdgeRadius checkOutlineColour
checkBackground = checkArms & checkCenter & checkOutline

-- final assembly
check :: Picture
check = translated checkOffsetX checkOffsetY 
  (dilated checkScale (checkArms & checkBackground))


------------
-- PLAYER --
------------
-- colours
torsoColour, skinColour, eyeColour :: Colour
torsoColour = fromRGB 25 142 191
skinColour = fromRGB 255 222 194
eyeColour = fromRGB 61 61 61

helmetShellColour, helmetStripeColour, helmetCapColour :: Colour
helmetShellColour = fromRGB 255 204 0
helmetStripeColour = fromRGB 255 213 46
helmetCapColour = fromRGB 222 178 6

shoeColour, sleeveColour, pocketColour :: Colour
shoeColour = fromRGB 73 73 73
sleeveColour = fromRGB 22 128 173
pocketColour = fromRGB 16 113 154

-- dimensions
torsoRadius, torsoOffsetY, faceRadius, faceOffsetY :: Double
torsoRadius = 0.25
torsoOffsetY = -0.1 -- negative value
faceRadius = 0.2
faceOffsetY = 0.15

eyeRadius, eyeScaleX, eyeScaleY, eyeOffsetX, eyeOffsetY :: Double
eyeRadius = 0.05
eyeScaleX = 1.0
eyeScaleY = 1.75
eyeOffsetX = 0.0875
eyeOffsetY = 0.1125

helmetRadius, helmetOffsetY, helmetCapRadius :: Double
helmetRadius = faceRadius + 0.025
helmetOffsetY = faceOffsetY
helmetCapRadius = 0.025

shoeRadius, shoeScaleX, shoeOffsetX, shoeOffsetY :: Double
shoeRadius = 0.075
shoeScaleX = 2.0
shoeOffsetX = 0.1
shoeOffsetY = torsoOffsetY - torsoRadius -- negative value

sleeveRadius, sleeveScaleY, handRadius, handOffsetY :: Double
sleeveRadius = 0.0625
sleeveScaleY = 1.875
handRadius = 0.05
handOffsetY = -sleeveRadius * sleeveScaleY - handRadius -- negative value

armOffsetX, armOffsetY, armAngle :: Double
armOffsetX = torsoRadius - sleeveRadius / 2.0
armOffsetY = torsoOffsetY + sleeveRadius / 2.0
armAngle = pi / 16

pocketRadius, pocketScaleX, pocketOffsetX, pocketOffsetY :: Double
pocketRadius = torsoRadius / 3.0
pocketScaleX = 0.5
pocketOffsetX = torsoRadius / 3.0
pocketOffsetY = torsoOffsetY - torsoRadius / 3.0 -- negative value

-- drawing shared & basic segments
torso, face, eye :: Picture
torso = translated 0.0 torsoOffsetY (coloured torsoColour (solidCircle torsoRadius))
face = translated 0.0 faceOffsetY (coloured skinColour (solidCircle faceRadius))
eye = scaled 1.0 eyeScaleY (coloured eyeColour (solidCircle eyeRadius))

helmetShell, helmetStripe, helmetCap, helmet :: Picture
helmetShell = coloured helmetShellColour (sector 0 pi helmetRadius)
helmetStripe = translated 0.0 (helmetRadius / 2.0) 
  (roundedRectangle 0.0 helmetRadius helmetCapRadius helmetStripeColour)
helmetCap = roundedRectangle (2.0 * helmetRadius) 0.0 helmetCapRadius helmetCapColour
helmet = translated 0.0 helmetOffsetY (helmetCap & helmetStripe & helmetShell)

shoe, sleeve, hand, arm :: Picture
shoe = coloured shoeColour (solidCircle shoeRadius)
sleeve = scaled 1.0 sleeveScaleY (coloured sleeveColour (solidCircle sleeveRadius))
hand = coloured skinColour (solidCircle handRadius)
arm = (translated 0.0 handOffsetY hand) & sleeve

-- drawing playerU & playerD segments (they use similar segments in different orders)
eyesUD, shoesUD, armUD, armsUD, pocketU, pocketsU :: Picture
eyesUD = pictures[translated x eyeOffsetY eye | x <- [-eyeOffsetX, eyeOffsetX] ]
shoesUD = pictures[translated x shoeOffsetY shoe | x <- [-shoeOffsetX, shoeOffsetX] ]
armUD = translated armOffsetX armOffsetY (rotated armAngle arm)
armsUD = armUD & (reflected (pi / 2) armUD)
pocketU = scaled pocketScaleX 1.0 (coloured pocketColour (sector pi (2 * pi) pocketRadius))
pocketsU = pictures[translated x pocketOffsetY pocketU 
  | x <- [-pocketOffsetX, pocketOffsetX] ]

-- drawing playerR & playerL segments (playerL is an identical reflection)
eyesR, shoeFrontR, shoeBackR, armsR :: Picture
eyesR = translated eyeOffsetX eyeOffsetY eye
shoeFrontR = translated (1.75 * shoeOffsetX) (0.85 * shoeOffsetY)
  (rotated (pi / 12) (scaled 1.5 0.75 shoe))
shoeBackR = reflected (pi / 2) shoeFrontR
armsR = translated 0.0 armOffsetY (rotated (-armAngle) arm)

-- final assembly
playerR, playerU, playerL, playerD :: Picture
playerR = helmet & eyesR & face & armsR & shoeBackR & torso & shoeFrontR
playerU = pocketsU & helmet & face & torso & shoesUD & armsUD
playerL = reflected (pi / 2) playerR
playerD = helmet & eyesUD & face & armsUD & shoesUD & torso


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
