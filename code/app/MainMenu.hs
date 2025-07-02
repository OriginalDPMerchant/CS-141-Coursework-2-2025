module MainMenu where

import Types
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Util
import System.Exit
import Graphics.Vty
import Control.Monad.IO.Class
 
--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleMainMenuEvent :: BrickEvent WidgetID GameState -> GameUpdate

-- Exit the Application when the Quit Button is Pressed
handleMainMenuEvent (MouseDown QuitBtn BLeft _ _) = liftIO exitSuccess

-- Ignore all other Events
handleMainMenuEvent _ = pure ()

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Render MainMenu
drawMainMenu :: Widget WidgetID
drawMainMenu = 
    border $ vCenter $ vBox 
    [ 
        hCenter $ hLimit 80 $ hBorder,
        str "\n",
        drawTitle, 
        str "\n",
        hCenter $ hLimit 80 $ hBorder,
        str "\n\n",
        hCenter $ hLimit 70 $ drawButton playBtnID greenBGAttr "Play",
        str "\n",
        hCenter $ hLimit 70 $ drawButton (ChangeGameModeBtn LoadGameMenu) blueBGAttr "Load Game",
        str "\n",
        hCenter $ hLimit 70 $ drawButton (ChangeGameModeBtn Instructions) brightBlueBGAttr "Instructions",
        str "\n",
        hCenter $ hLimit 70 $ drawButton QuitBtn redBGAttr "Quit",
        str "\n",
        hCenter $ hLimit 80 $ hBorder 
    ]
    where playBtnID = (ChangeGameModeBtn (PlayerNameEntry 0 (PlayerNameEntry 1 GameSettingsMenu)))