module GameSettingsMenu where

import Types
import Gridlock.Types
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util
import Graphics.Vty
import Gridlock.DrawGrid

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleGameSettingsMenuEvent :: BrickEvent WidgetID GameState -> GameUpdate

-- When the Arrow is pressed increment / decrement the GridWidth
handleGameSettingsMenuEvent (MouseDown (GridWidthBtn x) BLeft _ _) =
    modify $ \s -> s 
    { 
        gridWidth = max 1 (min (maxGridSize s) (gridWidth s + x)) 
    }

-- When the Arrow is pressed increment / decrement the GridHeight
handleGameSettingsMenuEvent (MouseDown (GridHeightBtn x) BLeft _ _) = 
    modify $ \s -> s 
    { 
        gridHeight = max 1 (min (maxGridSize s) (gridHeight s + x)) 
    }

-- When the Arrow is pressed toggle whether a colour is enabled or disabled
handleGameSettingsMenuEvent (MouseDown (EnableColourBtn colour) BLeft _ _) = 
    modify $ \s -> s 
    {
        enabledColours = case Map.lookup colour (enabledColours s) of
                            (Just x) -> Map.insert colour (not x) (enabledColours s)
                            _        -> (enabledColours s)
    }

-- When the Start Game Button is pressed transition to a new Gridlock Game
handleGameSettingsMenuEvent (MouseDown (StartGameBtn) BLeft _ _) = do
    gameState <- get

    let record = makeNewGameRecord gameState
    
    let colourList = getColourList gameState

    let updateStateNoColours = modify $ \s -> s { 
        gameMode = ErrorScreen "No Colours have been enabled. Please Enable at least one colour to play" GameSettingsMenu
    }

    let updateStateSameName = modify $ \s -> s { 
        gameMode = ErrorScreen "Both Players have the same name" GameSettingsMenu
    }

    let updateStateValid = modify $ \s -> s {
        gameMode = GridlockGame,
        turnIndex = 0,
        maxTurnIndex = 0,
        selectedColour = head $ getColourList s,
        hasGameEnded = False,
        logOutput = unlines
            [
                "Begin Gridlock Game!\n",
                printGameInfo record,
                printTurnString record 0 ++ "\n",
                drawGridNoAnsi (last $ grids record)
            ],
        gridlockGameRecord = (Just record)
    }

    if null colourList then
        updateStateNoColours
    else
        if playerNames gameState !! 0 == playerNames gameState !! 1 then
            updateStateSameName
        else
            updateStateValid

-- Ignore all other Events
handleGameSettingsMenuEvent _ = pure ()

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

-- Make the GameRecord for the new game
makeNewGameRecord :: GameState -> GameRecord
makeNewGameRecord gameState = 
    GameRecord
    {
        grids = [Grid newGridWidth newGridHeight gridMap],
        players = playerNames gameState,
        colours = Set.fromList (getColourList gameState)
    }
    where newGridWidth  = gridWidth gameState
          newGridHeight = gridHeight gameState
          gridMap       = Map.fromList [((x, y), Empty) | x <- [0 .. newGridWidth - 1], y <- [0 .. newGridHeight - 1]]

-- Get the List of enabled colours
getColourList :: GameState -> [Colour]
getColourList gameState    = filter wasColourPicked [Red .. Cyan]
    where wasColourPicked colour = case Map.lookup colour (enabledColours gameState) of
                                (Just x) -> x
                                _        -> False

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Render Game Settings Menu
drawGameSettingsMenu :: GameState -> Widget WidgetID
drawGameSettingsMenu gameState = 
    border $ vCenter $ vBox 
    [ 
        hCenter $ hLimit 80 $ hBorder,
        str "\n",
        drawTitle, 
        str "\n",
        hCenter $ hLimit 80 $ hBorder,
        str "\n\n",
        hCenter $ hLimit 70 $ hBox 
        [
            str "\nColours : ",
            vBox (map (`drawColourEnableWidget` gameState) [Red .. Cyan])
        ],
        str "\n",
        hCenter $ hLimit 70 $ hBox 
        [
            str "Grid Width :",
            clickable (GridWidthBtn (-1)) $ str " < ",
            str (show $ gridWidth gameState),
            clickable (GridWidthBtn 1) $ str " > "
        ],
        str "\n",
        hCenter $ hLimit 70 $ hBox 
        [
            str "Grid Height :",
            clickable (GridHeightBtn (-1)) $ str " < ",
            str (show $ gridHeight gameState),
            clickable (GridHeightBtn 1) $ str " > "
        ],
        str "\n",
        hCenter $ hLimit 70 $ hBox
        [ 
            str " ", 
            drawButton (ChangeGameModeBtn MainMenu) redBGAttr "Cancel", 
            str " ", 
            drawButton StartGameBtn greenBGAttr "Start Game", 
            str " "
        ],
        str "\n",
        hCenter $ hLimit 80 $ hBorder
    ]

-- Draw The widget which allows us to specity whether a colour is enabled or disabled in the game
drawColourEnableWidget :: Colour -> GameState -> Widget WidgetID
drawColourEnableWidget colour gameState = 
    hBox
    [
        withAttr (cellAttr colour) $ str fullCellImage,
        clickable (EnableColourBtn colour) $ str "\n < ",
        currentSetting,
        clickable (EnableColourBtn colour) $ str "\n > "
    ]
    where currentSetting = case Map.lookup colour (enabledColours gameState) of
                            (Just True)  -> withAttr (cellAttr Green) $ str "\nEnabled"
                            (Just False) -> withAttr (cellAttr Red) $ str "\nDisabled"
                            _            -> withAttr (cellAttr Red) $ str "\nUnknown"