module PlayerNameEntry where

import Types
import Gridlock.Types
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Util
import Graphics.Vty
import ErrorScreen
import Data.Char

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handlePlayerNameEntryEvent :: BrickEvent WidgetID GameState -> GameUpdate

-- When any char key is pressed then add that character to the end of the file path
handlePlayerNameEntryEvent (Brick.VtyEvent (EvKey (KChar c) [])) = 
    modify $ \s -> s 
    {
        playerNames = updatePlayerNames s ((getPlayerName s) ++ [c])
    }

-- When backspace is pressed delete last character from the file path
handlePlayerNameEntryEvent (Brick.VtyEvent (EvKey KBS [])) = 
    modify $ \s -> s 
    { 
        playerNames = updatePlayerNames s (performBackspace $ getPlayerName s)
    }

-- When Enter is pressed submit the entered name
handlePlayerNameEntryEvent (Brick.VtyEvent (EvKey KEnter [])) = 
    modify $ \s -> s 
    {
        gameMode = case gameMode s of
                        (PlayerNameEntry _ mode) -> if (isNameValid $ getPlayerName s) then mode else gameMode s
                        _                        -> gameMode s

    }

-- Ignore all other Events
handlePlayerNameEntryEvent _ = pure ()

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

-- returns the updated array of player names when we set the Name of the player represented by the current index of the PlayerNameEntry Game Mode
updatePlayerNames :: GameState -> Player -> [Player]
updatePlayerNames gameState newName = case gameMode gameState of
                                        (PlayerNameEntry index _) -> setListElement index newName (playerNames gameState)
                                        _                         -> (playerNames gameState)

-- returns the Name of the player represented by the current index of the PlayerNameEntry Game Mode
getPlayerName :: GameState -> Player
getPlayerName gameState = case gameMode gameState of
                                (PlayerNameEntry index _) -> (playerNames gameState) !! index
                                _                         -> ""

-- Checks that a player name is valid (no spaces no non alpha-numeric characters)
isNameValid :: Player -> Bool
isNameValid name = not (null name) && all isAlphaNum name

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Render the Name Entry Screen or the Error Screen depending on if we are in the correct gameMode or not
drawPlayerNameEntry :: GameState -> Widget WidgetID
drawPlayerNameEntry gameState = case gameMode gameState of
                                    (PlayerNameEntry index mode) -> drawPlayerNameEntryHelper ((playerNames gameState) !! index) (index + 1) mode
                                    _                            -> drawErrorScreen gameState

-- Render the Name Entry Screen
drawPlayerNameEntryHelper :: Player -> Int -> GameMode -> Widget WidgetID
drawPlayerNameEntryHelper player index mode = 
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
            str ("Enter Player " ++ show index ++ " Name : "),
            withAttr selectedFileAttr $ str player,
            str "█"
        ],
        str "\n",
        hCenter $ hLimit 70 $ hBox
        [
            str " ",
            drawButton btnId btnAttr "Continue",
            str " "
        ],
        str "\n",
        hCenter $ hLimit 80 $ hBorder
    ]
    where btnId = if isNameValid player then (ChangeGameModeBtn mode) else None
          btnAttr = if isNameValid player then greenBGAttr else greyBGAttr