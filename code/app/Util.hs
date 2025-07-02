module Util where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Gridlock.ColourSquares
import Gridlock.Types
import Types
import Graphics.Vty.Attributes
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import System.FilePath
import Control.Monad.IO.Class
import System.Directory
import Control.Exception

--                          Event Handling
-- //////////////////////////////////////////////////////////////

handleFileEntryClickedEvent :: FilePath -> Bool -> GameUpdate
handleFileEntryClickedEvent fileName showDirectory = do
    gameState <- get
    
    let fullFilePath = currentDirectory gameState </> fileName

    let updateFile _ = modify $ \s -> s { selectedFilePath = fullFilePath }

    let updateDirectory subDir = modify $ \s -> s {
        currentDirectory = fullFilePath, 
        subDirectories = subDir,
        selectedFilePath = if showDirectory then fullFilePath else selectedFilePath s
    }

    let getDirectoryOrFileFailure err = modify $ \s -> s { gameMode = ErrorScreen (show err) LoadGameMenu }

    let getDirectoryOrFileSuccess value = either updateDirectory updateFile value

    result <- liftIO $ try (getDirectoryOrFile fullFilePath) :: EventM WidgetID GameState (Either IOException (Either [FilePath] FilePath))

    either getDirectoryOrFileFailure getDirectoryOrFileSuccess result

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

-- Helper function that returns the sub directories if the file path is a directoy, returns the file parth if its a file, otherwise it returns Nothing
getDirectoryOrFile :: FilePath -> IO (Either [FilePath] FilePath)
getDirectoryOrFile path = do
    isDir <- doesDirectoryExist path
    
    if isDir then do
        subDir <- listDirectory path
        return $ Left subDir
    else do
        isFile <- doesFileExist path
        if isFile then 
            return $ Right path
        else 
            throwIO (userError $ "File or directory does not exist: " ++ path)

-- Helper function to update the value at a certain index of a list            
setListElement :: Int -> a -> [a] -> [a]
setListElement _ _ [] = []
setListElement 0 newVal (_:xs) = newVal : xs
setListElement n newVal (x:xs) = x : setListElement (n - 1) newVal xs

-- Helper function which mimicks what happens when you press the backspace button
performBackspace :: String -> String
performBackspace s = if null s then "" else init s

--      Helper function to format the game records information as a string to be displayed in the log
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
printGameInfo :: GameRecord -> String
printGameInfo record = unlines     
    [
        logoImage,
        printPlayers record, 
        printColours record, 
        printGridSize $ last $ grids record
    ]

printGameEnd :: GameRecord -> String
printGameEnd record = unlines
    [
        printLoser record ++ "\n",
        winnerImage,
        printWinner record
    ]

printPlayers :: GameRecord -> String
printPlayers record = "Players : " ++ intercalate ", " (players record) 

printColours :: GameRecord -> String
printColours record = "Colours : " ++ intercalate ", " (map show (Set.toList $ colours record))

printGridSize :: Grid -> String
printGridSize grid = "Grid Size : " ++ show (width grid) ++ "x" ++ show (height grid)

printTurnString :: GameRecord -> Int -> String
printTurnString record index = "Turn Index : " ++ show index ++ "\n" ++ prevPlayer ++ " | " ++ nextPlayer
    where nextPlayer = "Next Player : " ++ getCurrentPlayer record index
          prevPlayer = if index == 0 then "Prev Player : N/A" else "Prev Player : " ++ getCurrentPlayer record (index - 1)

printLoser :: GameRecord -> String
printLoser record = getCurrentPlayer record ((length $ grids record) - 1) ++ " Cannot Move!"

printWinner :: GameRecord -> String
printWinner record = getCurrentPlayer record (length $ grids record) ++ " Is the Winner!!!"

-- Gets the Player whose turn it should be 
getCurrentPlayer :: GameRecord -> Int -> Player
getCurrentPlayer record index = (players record) !! (mod index (length $ players record))

--                          Widgets
-- //////////////////////////////////////////////////////////////

-- Draws a Clickable Button
drawButton :: WidgetID -> AttrName -> String -> Widget WidgetID
drawButton widgetID attribute text = clickable widgetID $ withAttr attribute $ padAll 1 $ hCenter $ str text

-- Draws the Title Widget
drawTitle :: Widget WidgetID
drawTitle = 
    vBox
    [
        withAttr (cellAttr Cyan) $ hCenter $ str logoImage,
        hCenter $ str "Programmed by Oluwaferanmi Akodu"
    ]

-- Draw the file exploer widget
drawFileBrowser :: GameState -> Widget WidgetID
drawFileBrowser gameState = 
    border $ withVScrollBars OnRight $ viewport FileBrowserViewport Vertical $ vBox
    [ 
        drawFileEntry (takeDirectory (currentDirectory gameState)) 0,
        hBox 
        [
            str "  ├─", 
            withAttr currentDirectoryAttr $ str (currentDirectory gameState)
        ],
        vBox (map (`drawFileEntry` 2) (subDirectories gameState))
    ]

-- Draw the widget for a file/directory in the file explorer
drawFileEntry :: FilePath -> Int -> Widget WidgetID
drawFileEntry filePath indentation = clickable (FileBrowserEntry filePath) $ str ((replicate (2 * indentation) ' ') ++ "├─" ++ filePath)

--                          Attributes
-- //////////////////////////////////////////////////////////////

currentDirectoryAttr :: AttrName
currentDirectoryAttr = attrName "currentDirectory"

selectedFileAttr :: AttrName
selectedFileAttr = attrName "selectedFile"

redBGAttr :: AttrName
redBGAttr = attrName "redBG"

greenBGAttr :: AttrName
greenBGAttr = attrName "greenBG"

blueBGAttr :: AttrName
blueBGAttr = attrName "blueBG"

brightMagentaBGAttr :: AttrName
brightMagentaBGAttr = attrName "brightMagentaBG"

brightBlueBGAttr :: AttrName
brightBlueBGAttr = attrName "brightBlueBG"

greyBGAttr :: AttrName
greyBGAttr = attrName "greyBG"

greyFGAttr :: AttrName
greyFGAttr = attrName "greyFG"

cellAttr :: Colour -> AttrName
cellAttr Red     = attrName "redCell"
cellAttr Green   = attrName "greenCell"
cellAttr Yellow  = attrName "yellowCell"
cellAttr Blue    = attrName "blueCell"
cellAttr Magenta = attrName "magentaCell"
cellAttr Cyan    = attrName "cyanCell"

-- Attribute Map
myAttrMap :: AttrMap
myAttrMap = 
    attrMap defAttr 
    [
        (currentDirectoryAttr, withStyle (fg red) bold),
        (selectedFileAttr, withStyle (fg green) bold),
        (redBGAttr, bg red),
        (greenBGAttr, bg green),
        (blueBGAttr, bg blue),
        (brightMagentaBGAttr, bg brightMagenta),
        (brightBlueBGAttr, bg brightBlue),
        (greyBGAttr, bg brightBlack),
        (greyFGAttr, fg brightBlack),
        (cellAttr Red, fg red),
        (cellAttr Green, fg green),
        (cellAttr Yellow, fg yellow),
        (cellAttr Blue, fg blue),
        (cellAttr Magenta, fg magenta),
        (cellAttr Cyan, fg cyan)
    ]

--                          Images
-- //////////////////////////////////////////////////////////////

logoImage :: String
logoImage = unlines [ " ██████╗ ██████╗ ██╗██████╗ ██╗      ██████╗  ██████╗██╗  ██╗"
                    , "██╔════╝ ██╔══██╗██║██╔═██╚╗██║     ██╔═══██╗██╔════╝██║ ██╔╝"
                    , "██║  ███╗██████╔╝██║██║  ██║██║     ██║   ██║██║     █████╔╝ "
                    , "██║   ██║██╔══██╗██║██║  ██║██║     ██║   ██║██║     ██╔═██╗ "
                    , "╚██████╔╝██║  ██║██║██████╔╝███████╗╚██████╔╝╚██████╗██║  ██╗"
                    , " ╚═════╝ ╚═╝  ╚═╝╚═╝╚═════╝ ╚══════╝ ╚═════╝  ╚═════╝╚═╝  ╚═╝"]

winnerImage :: String              
winnerImage = unlines [ "██╗    ██╗██╗███╗   ██╗███╗   ██╗███████╗██████╗     ██╗"
                      , "██║    ██║██║████╗  ██║████╗  ██║██╔════╝██╔══██╗    ██║"
                      , "██║ █╗ ██║██║██╔██╗ ██║██╔██╗ ██║█████╗  ██████╔╝    ██║"
                      , "██║███╗██║██║██║╚██╗██║██║╚██╗██║██╔══╝  ██╔══██╗    ╚═╝"
                      , "╚███╔███╔╝██║██║ ╚████║██║ ╚████║███████╗██║  ██║    ██╗"
                      , " ╚══╝╚══╝ ╚═╝╚═╝  ╚═══╝╚═╝  ╚═══╝╚══════╝╚═╝  ╚═╝    ╚═╝"]

fullCellImage :: String
fullCellImage = unlines [ "████╗"
                        , "████║"
                        , "╚═══╝"]

emptyCellImage :: String
emptyCellImage = unlines [ "╔═══╗"
                         , "║   ║"
                         , "╚═══╝"]