module GridlockGame where

import Types
import Gridlock.Types
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Table
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util
import Gridlock.ColourSquares
import Graphics.Vty
import Control.Monad
import Gridlock.DrawGrid
import Gridlock.GameLogic

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleGridlockGameEvent :: BrickEvent WidgetID GameState -> GameUpdate

-- Increment / Decrement the turn Index when next move / prev move button is pressed
handleGridlockGameEvent (MouseDown (IncrementMoveBtn x) BLeft _ _) = 
    modify $ \s -> s 
    {
        turnIndex = max 0 (min (maxTurnIndex s) (turnIndex s + x))
    }

-- Update the selected colour when the colour select button is pressed
handleGridlockGameEvent (MouseDown (ColourSelectBtn colour) BLeft _ _) = modify $ \s -> s 
    {
        selectedColour = colour
    }

-- Play move when a grid cell is clicked
handleGridlockGameEvent (MouseDown (GridCell coord) BLeft _ _) = do
    gameState <- get

    let player record = getCurrentPlayer record (turnIndex gameState)
    let colour = selectedColour gameState
    
    let attemptMove record = playMove (GameRecordExt True "" "" record) (GridlockMove (player record) colour coord)

    let updateStateInvalid err = modify $ \s -> s { 
        logOutput = logOutput s ++ "\n" ++ err ++ "\n",
        gameMode = ErrorScreen err GridlockGame
    }

    let updateStateValid record = modify $ \s -> s {
        logOutput = unlines
        [
            logOutput s,
            printTurnString record (turnIndex s + 1) ++ "\n",
            drawGridNoAnsi (last $ grids record)
        ],
        gridlockGameRecord = Just record,
        turnIndex = max 0 ((length $ grids record) - 1),
        maxTurnIndex = max 0 ((length $ grids record) - 1)
    }

    let isGameOver record = not $ possibleMovesExist (GameRecordExt True "" "" record) (player record)
    
    let endGame record = modify $ \s -> s {
        hasGameEnded = True,
        logOutput = unlines
        [
            logOutput s,
            printGameEnd record
        ]
    }

    let updateState record =
         case attemptMove record of 
            GameRecordExt { isValid = True, gameRecord = r } -> do
                updateStateValid r
                when (isGameOver r) $ endGame r
                vScrollToEnd (viewportScroll GameLogViewport)
            GameRecordExt { isValid = False, errorMsg = err }     -> do
                updateStateInvalid err
                vScrollToEnd (viewportScroll GameLogViewport)

    maybe (return ()) updateState (gridlockGameRecord gameState)

-- Handle using the Scroll wheel to scroll the file selection menu
handleGridlockGameEvent (Brick.VtyEvent (EvMouseDown _ _ BScrollDown _)) = vScrollBy (viewportScroll GameLogViewport) 1
handleGridlockGameEvent (Brick.VtyEvent (EvMouseDown _ _ BScrollUp _)) = vScrollBy (viewportScroll GameLogViewport) (-1)

handleGridlockGameEvent (MouseDown GameLogViewport BScrollDown _ _) = vScrollBy (viewportScroll GameLogViewport) 1
handleGridlockGameEvent (MouseDown GameLogViewport BScrollUp _ _) = vScrollBy (viewportScroll GameLogViewport) (-1)

-- Ignore all other Events
handleGridlockGameEvent _ = pure ()

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

canModifyGrid :: GameState -> Bool
canModifyGrid gameState = not (hasGameEnded gameState) && (turnIndex gameState == maxTurnIndex gameState)

--                          Widgets
-- //////////////////////////////////////////////////////////////

drawGridlock :: GameState -> Widget WidgetID
drawGridlock gameState = 
    border $ vBox 
    [ 
        drawTitle,
        hBorder,
        hBox 
        [
            str "Selected File : ", 
            withAttr selectedFileAttr $ str (selectedFilePath gameState)
        ],
        hBox 
        [
            drawGameWindow gameState,
            vBox
            [
                drawGameInfoWindow gameState,
                drawGameLog gameState
            ]
        ],
        hBorder,
        hBox 
        [ 
            str " ", 
            drawButton (ChangeGameModeBtn MainMenu) redBGAttr "Quit", 
            str " ", 
            drawButton saveFileID saveFileAttr "Save", 
            str " "
        ]
    ]
    where saveFileAttr = if (hasGameEnded gameState) then brightBlueBGAttr else greyBGAttr
          saveFileID   = if (hasGameEnded gameState) then (ChangeGameModeBtn SaveGameMenu) else None

-- Draw the Log Window for the game
drawGameLog :: GameState -> Widget WidgetID
drawGameLog gameState = 
    border $ vBox 
    [ 
        hCenter $ str "Game Log",
        hBorder,
        withVScrollBars OnRight $ viewport GameLogViewport Vertical $ vBox
        [
            str (logOutput gameState)
        ]
    ]

drawGameInfoWindow :: GameState -> Widget WidgetID

-- Draw the Game Info Window then the Game Record is not Nothing
drawGameInfoWindow GameState {gridlockGameRecord = record} = 
     border $ vBox 
          [
               hCenter $ str "Game Info", 
               hBorder, 
               str $ (maybe "Players : N/A" printPlayers record), 
               str $ (maybe "Colours : N/A" printColours record), 
               str $ (maybe "Grid Size : N/A" (\x -> printGridSize $ last $ grids x) record)
          ]

-- Draw the Game Window for Gridlock
drawGameWindow :: GameState -> Widget WidgetID
drawGameWindow gameState =
    border $ vBox 
    [
        hCenter $ str "Game Window", 
        hBorder,
        drawPlayerTurnString gameState,
        hBox
        [
            str "Selected Colour : ",
            withAttr (cellAttr (selectedColour gameState)) $ str (show (selectedColour gameState))
        ],
        drawCurrentGrid gameState,
        hBorder,
        hCenter $ hBox 
        [ 
            str "\nColour Select : ",
            drawColourSelect gameState
        ],
        hBox 
        [ 
            str " ", 
            drawButton (IncrementMoveBtn (-1)) (prevMoveBtnAttr gameState) "Prev Move", 
            str " ",
            drawButton (IncrementMoveBtn 1) (nextMoveBtnAttr gameState) "Next Move",
            str " "
        ]
    ]

-- Draw the Grid of the Current Turn
drawCurrentGrid :: GameState -> Widget WidgetID
drawCurrentGrid gameState | height grid < 1 || width grid < 1 = fill ' '
                          | otherwise                         = vCenter $ hCenter $ drawGridWidget grid fullCellImage emptyCellImage (canModifyGrid gameState)
     where grid = case gridlockGameRecord gameState of
                    (Just record) -> (grids record) !! (turnIndex gameState)
                    _             -> Grid 3 3 (Map.fromList [((x, y), Empty) | x <- [0 .. 2], y <- [0 .. 2]])

-- Draw the widget for an arbitrary Grid
drawGridWidget :: Grid -> String -> String -> Bool -> Widget WidgetID
drawGridWidget grid filledImage emptyImage canModify = renderTable $ surroundingBorder False (table $ map row [0 .. height grid - 1])
    where row y        = map (`readCell` y) [0 .. width grid - 1]
          readCell x y = drawCellWidget (Map.lookup (x, y) (rep grid)) (x,y) (filledImage) (emptyImage) canModify

-- Draw the Cell of a Grid
drawCellWidget :: Maybe Cell -> Coord -> String -> String-> Bool -> Widget WidgetID 
drawCellWidget (Just (Filled colour)) (_, _) filledImage _ _ = withAttr (cellAttr colour) $ str filledImage

drawCellWidget _ (x,y) _ emptyImage canClick = clickable cellID $ str emptyImage
    where cellID = if canClick then (GridCell (x,y)) else None

-- Draw the string which tells us which player should play
drawPlayerTurnString :: GameState -> Widget WidgetID
drawPlayerTurnString gameState = str playerTurnString
     where playerTurnString = case gridlockGameRecord gameState of
                                   (Just record) -> printTurnString record (turnIndex gameState)
                                   _             -> "Turn Index : N/A | Player : N/A"

-- Draw the Colour Selection Menu
drawColourSelect :: GameState -> Widget WidgetID
drawColourSelect gameState = case gridlockGameRecord gameState of
                                   (Just record) -> hBox $ map (`drawColourSelectButton` (canModifyGrid gameState)) (Set.toList (colours record))
                                   _             -> hBox $ map (`drawColourSelectButton` False) [Red .. Cyan]

-- Draw a Button in the Colour Select Menu
drawColourSelectButton :: Colour -> Bool -> Widget WidgetID
drawColourSelectButton colour isEnabled = 
    hBox 
    [ 
        str " ", 
        clickable colourSelectID $ withAttr colourSelectAttr $ str fullCellImage,
        str " "
    ]
    where colourSelectAttr = if isEnabled then cellAttr colour else greyFGAttr
          colourSelectID   = if isEnabled then (ColourSelectBtn colour) else None

-- Returns the correct attribute so that the next move button is greyed out if it shouldnt be pressed
nextMoveBtnAttr :: GameState -> AttrName
nextMoveBtnAttr gameState | turnIndex gameState < maxTurnIndex gameState = greenBGAttr
                          | otherwise                                    = greyBGAttr

-- Returns the correct attribute so that the prev move button is greyed out if it shouldnt be pressed
prevMoveBtnAttr :: GameState -> AttrName
prevMoveBtnAttr gameState | turnIndex gameState > 0 = redBGAttr
                          | otherwise               = greyBGAttr