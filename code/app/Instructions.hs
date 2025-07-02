module Instructions where

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

handleInstructionsEvent :: BrickEvent WidgetID GameState -> GameUpdate

-- Ignore all other Events
handleInstructionsEvent _ = pure ()

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

instructions :: String
instructions = unlines [ "Description"
                         , "-----------"
                         , ""
                         , "    - Gridlock is a two-player strategic coloring game played on a grid. The goal is to be the last player to make a valid move."
                         , ""
                         , "Gameplay"
                         , "--------"
                         , ""
                         , "    - Players take turns coloring one cell of the grid per turn"
                         , "    - Players select the colour to play by clicking on the corresponding coloured square"
                         , "    - Players coluor cells by clicking on the cell that they wish to colour"
                         , "    - When coloring a cell, the following rules must be followed:"
                         , "          - The cell must be empty (i.e., not already colored)."
                         , "          - The color chosen must not be the same as any adjacent cell (cells that are directly above, below, to the left, or to the right)."
                         , "Winning"
                         , "-------"
                         , ""
                         , "   - The game ends when a player cannot make a valid move on their turn."
                         , "   - The player who made the last valid move wins the game."
                         , ""
                         , "Notes"
                         , "-----"
                         , ""
                         , "   - Press Escape to exit the application at any stage"
                         , "   - This program was written in haskell as a submission for CS141 functional programming coursework 2 in 2025 at the University of Warwick"
                         ]

--                          Widgets
-- //////////////////////////////////////////////////////////////
drawInstructions :: Widget WidgetID
drawInstructions =
    border $ vBox 
    [
        drawTitle,
        hBorder,
        strWrap instructions,
        fill ' ',
        hBorder,
        hBox
        [
            str " ",
            drawButton (ChangeGameModeBtn MainMenu) greenBGAttr "Main Menu",
            str " "
        ]
    ]
