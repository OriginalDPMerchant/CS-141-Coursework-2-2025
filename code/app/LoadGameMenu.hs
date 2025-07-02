module LoadGameMenu where

import Types
import Gridlock.Types
import Brick
import Brick.Widgets.Border
import Util
import Graphics.Vty
import Control.Monad.IO.Class
import qualified Text.Megaparsec as Megaparsec
import Gridlock.DrawGrid
import Gridlock.Parser
import Control.Exception 

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleLoadGameMenuEvents :: BrickEvent WidgetID GameState -> GameUpdate

-- When any char key is pressed then add that character to the end of the file path
handleLoadGameMenuEvents (Brick.VtyEvent (EvKey (KChar c) [])) = 
    modify $ \s -> s 
    { 
        selectedFilePath = (selectedFilePath s) ++ [c] 
    }

-- When backspace is pressed delete last character from the file path
handleLoadGameMenuEvents (Brick.VtyEvent (EvKey KBS [])) = 
    modify $ \s -> s 
    { 
        selectedFilePath = performBackspace $ selectedFilePath s
    }

-- Change the current directory if a user clicks on a folder ... if the user clicks on the files make that file the selected file
handleLoadGameMenuEvents (MouseDown (FileBrowserEntry fileName) BLeft _ _) = handleFileEntryClickedEvent fileName False

-- When Enter is pressed load the File
handleLoadGameMenuEvents (Brick.VtyEvent (EvKey KEnter [])) = loadGame

-- Attempt to load a gridlock file and display the game once the file has been loaded successfully
handleLoadGameMenuEvents (MouseDown LoadFileBtn BLeft _ _) = loadGame

-- Handle using the Scroll wheel to scroll the file selection menu
handleLoadGameMenuEvents (Brick.VtyEvent (EvMouseDown _ _ BScrollDown _)) = vScrollBy (viewportScroll FileBrowserViewport) 1
handleLoadGameMenuEvents (Brick.VtyEvent (EvMouseDown _ _ BScrollUp _)) = vScrollBy (viewportScroll FileBrowserViewport) (-1)

handleLoadGameMenuEvents (MouseDown FileBrowserViewport BScrollDown _ _) = vScrollBy (viewportScroll FileBrowserViewport) 1
handleLoadGameMenuEvents (MouseDown FileBrowserViewport BScrollUp _ _) = vScrollBy (viewportScroll FileBrowserViewport) (-1)

-- Ignore all other Events
handleLoadGameMenuEvents _ = pure ()

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

printPlayedMoves :: GameRecord -> String
printPlayedMoves record = unlines [printTurnString record i ++ "\n\n" ++ (drawGridNoAnsi x) ++ "\n" | (i, x) <- zip [0..] (grids record)]

loadGame :: GameUpdate    
loadGame = do

    let logError err = unlines [ "Failed To Load File!\n"
                               , Megaparsec.errorBundlePretty err ]

    let logSuccess record = unlines [ "Successfully loaded File!\n"
                                    , printGameInfo record
                                    , printPlayedMoves record
                                    , printGameEnd record ]

    let updateState result = modify $ \s -> s {
        gameMode = GridlockGame,
        gridlockGameRecord = either (const Nothing) Just result,
        turnIndex = 0,
        maxTurnIndex = either (const 0) (\g -> max 0 (length (grids g) - 1)) result,
        hasGameEnded = True,
        logOutput = either logError logSuccess result
    }
    
    let readFileFailiure err = modify $ \s -> s { gameMode = ErrorScreen (show err) LoadGameMenu }

    let readFileSuccess rawText = updateState (Megaparsec.parse parseGame "" rawText)
    
    gameState <- get
    result    <- liftIO $ try (readFile (selectedFilePath gameState)) :: EventM WidgetID GameState (Either IOException String)

    either readFileFailiure readFileSuccess result

    vScrollToBeginning (viewportScroll GameLogViewport)

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Render LoadGameMenu
drawLoadGameMenu :: GameState -> Widget WidgetID
drawLoadGameMenu gameState =
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
            drawButton (ChangeGameModeBtn MainMenu) redBGAttr "Cancel", 
            str " ", 
            drawButton LoadFileBtn brightBlueBGAttr "Load", 
            str " "
        ]
    ]