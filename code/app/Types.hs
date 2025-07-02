module Types where

import Gridlock.Types
import Brick
import Data.Map (Map)
import qualified Data.Map as Map

-- A WidgetID is a unique identifier for each interactable widget in the UI
data WidgetID
  = ChangeGameModeBtn GameMode
  | QuitBtn
  | SaveFileBtn
  | LoadFileBtn
  | StartGameBtn
  | FileBrowserEntry String
  | EnableColourBtn Colour
  | GridWidthBtn Int
  | GridHeightBtn Int
  | FileBrowserViewport
  | GameLogViewport
  | IncrementMoveBtn Int
  | GridCell Coord
  | ColourSelectBtn Colour
  | None
  deriving (Eq, Ord, Show)

-- represents which section of the application we are currently in
data GameMode 
    = MainMenu
    | Instructions
    | GridlockGame
    | LoadGameMenu
    | SaveGameMenu
    | GameSettingsMenu
    | PlayerNameEntry Int GameMode
    | ErrorScreen String GameMode
    deriving (Eq, Ord, Show)

-- A data type representing the current state of the application
data GameState = GameState
    {
        gridlockGameRecord :: Maybe GameRecord,
        turnIndex :: Int,
        maxTurnIndex :: Int,
        selectedColour :: Colour,
        hasGameEnded :: Bool,
        gameMode :: GameMode,
        currentDirectory :: FilePath,
        subDirectories :: [FilePath],
        selectedFilePath :: FilePath,
        logOutput :: String,
        enabledColours :: Map Colour Bool,
        gridWidth :: Int,
        gridHeight :: Int,
        maxGridSize :: Int,
        playerNames :: [Player]
    }

type GameUpdate = EventM WidgetID GameState ()