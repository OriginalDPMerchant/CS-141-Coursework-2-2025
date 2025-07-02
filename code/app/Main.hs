-- This is the main entry point for your Gridlock application.

import Types
import Gridlock.Types
import Brick
import Brick.BChan qualified as Brick
import System.Exit
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Control.Monad.IO.Class
import System.Directory
import Data.Map (Map)
import qualified Data.Map as Map
import MainMenu
import GridlockGame
import GameSettingsMenu
import LoadGameMenu
import ErrorScreen
import PlayerNameEntry
import SaveGameMenu
import Instructions
import Util

main :: IO ()
main = do
  gameState <- initState
  eventChannel <- Brick.newBChan 10

  let builder = mkVty defaultConfig
  vty <- builder

  _ <- customMainWithVty vty builder (Just eventChannel) app gameState

  exitSuccess          

--My bRICK aPPLICATION
app :: App GameState GameState WidgetID
app = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = do
              vty <- getVtyHandle
              let output = outputIface vty
              if supportsMode output Mouse
                then liftIO $ setMode output Mouse True
                else error "Your terminal does not support mouse events :("
  , appAttrMap = const myAttrMap
  }

--Make the Initial GameState
initState :: IO GameState
initState = do
  homeDir <- getHomeDirectory
  subDir  <-  listDirectory homeDir
  return $ GameState {
      gridlockGameRecord = Nothing,
      turnIndex = 0,
      maxTurnIndex = 0,
      selectedColour = Red,
      hasGameEnded = False,
      gameMode = MainMenu, 
      currentDirectory = homeDir, 
      subDirectories = subDir, 
      selectedFilePath = "",
      logOutput = replicate 100 '\n',
      enabledColours = Map.fromList (map (\x -> (x, True)) [Red .. Cyan]),
      gridHeight = 1,
      gridWidth = 1,
      maxGridSize  = 6,
      playerNames = ["", ""]
    }

drawUI :: GameState -> [Widget WidgetID]
drawUI gameState = case gameMode gameState of
                      MainMenu              -> pure $ drawMainMenu
                      Instructions          -> pure $ drawInstructions
                      GridlockGame          -> pure $ drawGridlock gameState
                      LoadGameMenu          -> pure $ drawLoadGameMenu gameState
                      SaveGameMenu          -> pure $ drawSaveGameMenu gameState
                      (PlayerNameEntry _ _) -> pure $ drawPlayerNameEntry gameState
                      GameSettingsMenu      -> pure $ drawGameSettingsMenu gameState
                      (ErrorScreen _ _)     -> pure $ drawErrorScreen gameState

handleEvent :: BrickEvent WidgetID GameState -> GameUpdate
-- Quit the game with the 'ESC' key
handleEvent (Brick.VtyEvent (EvKey KEsc [])) = liftIO exitSuccess

-- Return to Main Menu when Main Menu button is clicked
handleEvent (MouseDown (ChangeGameModeBtn mode) BLeft _ _) = modify $ \s -> s { gameMode = mode }

handleEvent event = do
    gameState <- get

    case gameMode gameState of
        MainMenu              -> handleMainMenuEvent event
        Instructions          -> handleInstructionsEvent event
        GridlockGame          -> handleGridlockGameEvent event
        LoadGameMenu          -> handleLoadGameMenuEvents event
        SaveGameMenu          -> handleSaveGameMenuEvents event
        (PlayerNameEntry _ _) -> handlePlayerNameEntryEvent event
        GameSettingsMenu      -> handleGameSettingsMenuEvent event
        (ErrorScreen _ _)     -> handleErrorScreenEvent event