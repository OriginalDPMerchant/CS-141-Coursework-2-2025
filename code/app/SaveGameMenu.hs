module SaveGameMenu where

import Types
import Gridlock.Types
import Gridlock.ColourSquares
import Brick
import Brick.Widgets.Border
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Util
import Graphics.Vty
import Control.Monad.IO.Class
import Control.Exception
import Data.Char
import Data.List

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleSaveGameMenuEvents :: BrickEvent WidgetID GameState -> GameUpdate

-- When any char key is pressed then add that character to the end of the file path
handleSaveGameMenuEvents (Brick.VtyEvent (EvKey (KChar c) [])) = 
    modify $ \s -> s 
    { 
        selectedFilePath = (selectedFilePath s) ++ [c] 
    }

-- When backspace is pressed delete last character from the file path
handleSaveGameMenuEvents (Brick.VtyEvent (EvKey KBS [])) = 
    modify $ \s -> s
    { 
        selectedFilePath = performBackspace $ selectedFilePath s
    }

-- Change the current directory if a user clicks on a folder ... if the user clicks on the files make that file the selected file
handleSaveGameMenuEvents (MouseDown (FileBrowserEntry fileName) BLeft _ _) = handleFileEntryClickedEvent fileName True

-- When Enter is pressed load the File
handleSaveGameMenuEvents (Brick.VtyEvent (EvKey KEnter [])) = saveGame

-- Attempt to load a gridlock file and display the game once the file has been loaded successfully
handleSaveGameMenuEvents (MouseDown SaveFileBtn BLeft _ _) = saveGame

-- Handle using the Scroll wheel to scroll the file selection menu
handleSaveGameMenuEvents (Brick.VtyEvent (EvMouseDown _ _ BScrollDown _)) = vScrollBy (viewportScroll FileBrowserViewport) 1
handleSaveGameMenuEvents (Brick.VtyEvent (EvMouseDown _ _ BScrollUp _)) = vScrollBy (viewportScroll FileBrowserViewport) (-1)

handleSaveGameMenuEvents (MouseDown FileBrowserViewport BScrollDown _ _) = vScrollBy (viewportScroll FileBrowserViewport) 1
handleSaveGameMenuEvents (MouseDown FileBrowserViewport BScrollUp _ _) = vScrollBy (viewportScroll FileBrowserViewport) (-1)

-- Ignore all other Events
handleSaveGameMenuEvents _ = pure ()

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

-- saves a game to the file
saveGame ::GameUpdate
saveGame = do
    let writeFileFailiure err = modify $ \s -> s { gameMode = ErrorScreen (show err) SaveGameMenu }

    let writeFileSuccess _ = modify $ \s -> s { gameMode = GridlockGame }

    gameState <- get

    case (gridlockGameRecord gameState) of
        (Just record) -> do
            result <- liftIO $ try (writeFile (selectedFilePath gameState) (writeGameRecord record)) :: EventM WidgetID GameState (Either IOException ())
            either writeFileFailiure writeFileSuccess result
        _ -> do 
            modify $ \s -> s { gameMode = ErrorScreen "GameRecord is equal to Nothing" SaveGameMenu }

-- converts a game record to a gridlock file and returns the result as a string
writeGameRecord :: GameRecord -> String
writeGameRecord record = unlines
    [
        "GRIDLOCK",
        "Players: " ++ intercalate ", " (players record) ++ ".",
        "Grid: "  ++ show (width grid) ++ "x" ++ show (height grid) ++ ".",
        "Colours: " ++ intercalate ", " colourList ++ ".\n",
        unlines moveStrings,
        getCurrentPlayer record ((length $ grids record) - 1) ++ " cannot move.",
        getCurrentPlayer record (length $ grids record) ++ " wins!"
    ]
    where grid        = last $ grids record
          gridPairs   = zip (grids record) (drop 1 (grids record))
          moves       = [getPlayedMove (getCurrentPlayer record i) prev next | (i, (prev, next)) <- zip [0..] gridPairs]
          moveStrings = [writeMove move | (Just move) <- moves]
          colourList  = map (\x -> map toLower (show x)) (Set.toList $ colours record)

-- writes a move as a string
writeMove :: GridlockMove -> String
writeMove (GridlockMove player colour (x, y)) = intercalate " " [player, "plays", colourString, "at", coordString]
    where colourString = map toLower (show colour)
          coordString  = "(" ++ show x ++ "," ++ show y ++ ")."

-- Deduces the move played by a player by looking at the cell that changes between 2 grids
getPlayedMove :: Player -> Grid -> Grid -> Maybe GridlockMove
getPlayedMove player prevGrid nextGrid | not $ null changedCells = move
                                       | otherwise               = Nothing
    where xs             = [0 .. width nextGrid - 1]
          ys             = [0 .. height nextGrid - 1]
          hasChanged x y = (Map.lookup (x, y) (rep prevGrid)) /= (Map.lookup (x, y) (rep nextGrid))
          changedCells   = [(x,y) | x <- xs, y <- ys, hasChanged x y]
          move           = case Map.lookup (head changedCells) (rep nextGrid) of
                                (Just (Filled colour)) -> Just (GridlockMove player colour (head changedCells))
                                _                      -> Nothing

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Render SaveGameMenu
drawSaveGameMenu :: GameState -> Widget WidgetID
drawSaveGameMenu gameState =
    border $ vBox 
    [
        drawTitle,
        hBorder,
        drawFileBrowser gameState,
        hBorder,
        hBox 
        [
            str "Selected File : ",
            withAttr selectedFileAttr $ str (selectedFilePath gameState),
            str "█"
        ],
        str "\n",
        hBox
        [ 
            str " ", 
            drawButton (ChangeGameModeBtn GridlockGame) redBGAttr "Cancel", 
            str " ", 
            drawButton SaveFileBtn brightBlueBGAttr "Save", 
            str " "
        ]
    ]