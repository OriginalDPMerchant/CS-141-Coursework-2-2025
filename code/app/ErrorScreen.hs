module ErrorScreen where

import Types
import Gridlock.Types
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Util

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleErrorScreenEvent :: BrickEvent WidgetID GameState -> GameUpdate

-- Ignore all other Events
handleErrorScreenEvent _ = pure ()

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Draws the UI for the Error screen
drawErrorScreen :: GameState -> Widget WidgetID
drawErrorScreen gameState = 
    border $ vCenter $ vBox
    [
        hCenter $ vLimit 20 $ hLimit 80 $ border $ vBox
        [
            withAttr redBGAttr $ hCenter $ str "Error",
            hBorder,
            padLeftRight 5 $ vCenter $ vBox
            [
                withAttr (cellAttr Red) $ str "Error:\n",
                hCenter $ strWrap $ fst errorState
            ],
            hBox
            [
                str " ",
                drawButton (ChangeGameModeBtn $ snd errorState) redBGAttr "Try Again",
                str " "
            ]
        ]
    ]
    where errorState = case gameMode gameState of
                            (ErrorScreen msg mode) -> (msg, mode)
                            _                      -> ("We aren't supposed to be here", MainMenu)