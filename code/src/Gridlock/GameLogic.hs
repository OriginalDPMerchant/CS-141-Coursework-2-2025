module Gridlock.GameLogic where

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Gridlock.Types

-- Attempt to play a move if it is legal, if not Set the isValid Flag to false and provide an error message
playMove :: GameRecordExt -> GridlockMove -> GameRecordExt
playMove record move | not $ isPlayerValid record move     = setPlayMoveError record move "the current player doesnt exist"
                     | not $ shouldPlayerPlay record move  = setPlayMoveError record move "the current player tried taking two consecutive turns"
                     | not $ isColourValid record move     = setPlayMoveError record move "the colour played is not being used in this game"
                     | not $ isMoveInBounds lastGrid move  = setPlayMoveError record move "the coordinates of the cell are out of bounds"
                     | not $ isCellEmpty lastGrid move     = setPlayMoveError record move "the cell already has already been filled with a colour"
                     | isAdjacentCellTheSame lastGrid move = setPlayMoveError record move "an adjacent cell already has the same colour"
                     | otherwise                           = playValidMove record move
    where lastGrid   = last (getGrids record)

-- Set the isValid Flag to false and provide an error message
setPlayMoveError :: GameRecordExt -> GridlockMove -> String -> GameRecordExt
setPlayMoveError record move message = record {isValid = False, errorMsg = msg}
    where msg = "Tried to play an invalid move.\n" ++ gridlockMoveToString move ++ ".\nThis move failed because " ++ message

-- Updates the Gamerecord after a valid move was played
playValidMove :: GameRecordExt -> GridlockMove -> GameRecordExt
playValidMove record (GridlockMove player a b) = (setGrids record newGrids) {lastPlayer = player}
    where lastGrid = last (getGrids record)
          newGrids = getGrids record ++ [updateGrid lastGrid (GridlockMove player a b)]

--                          Move Validation Functions
-- //////////////////////////////////////////////////////////////

-- Checks that the player exists
isPlayerValid :: GameRecordExt -> GridlockMove -> Bool
isPlayerValid record (GridlockMove player _ _) = doesPlayerExist record player

--Checks that it is actually the players turn
shouldPlayerPlay :: GameRecordExt -> GridlockMove -> Bool
shouldPlayerPlay record (GridlockMove player _ _) = lastPlayer record /= player

-- check that the colour is one that was declared in the game 
isColourValid :: GameRecordExt -> GridlockMove -> Bool
isColourValid record (GridlockMove _ colour _) = Set.member colour (getColours record)

-- checks that the move is not outside of the grid
isMoveInBounds :: Grid -> GridlockMove -> Bool
isMoveInBounds grid (GridlockMove _ _ position) = isCoordInBounds grid position

-- checks that the cell is empty before the move can be played
isCellEmpty :: Grid -> GridlockMove -> Bool
isCellEmpty grid (GridlockMove _ _ position) = isCellEqualTo grid position Empty

-- checks whether the cell next to the selected cell is the same colour
isAdjacentCellTheSame :: Grid -> GridlockMove -> Bool
isAdjacentCellTheSame grid (GridlockMove _ colour (x, y)) = any checkCell validCells
    where cells           = [(x + m, y + n) | m <- [-1 .. 1], n <- [-1 .. 1],  abs m /= abs n]
          validCells      = filter (isCoordInBounds grid) cells
          checkCell coord = isCellEqualTo grid coord (Filled colour)

-- checks if the player can still potentially make moves
possibleMovesExist :: GameRecordExt -> Player -> Bool
possibleMovesExist record player = any (isGameRecordValid . playMove record) moves
    where lastGrid = last (getGrids record)
          colours  = Set.toList $ getColours record
          xs       = [0 .. width lastGrid - 1]
          ys       = [0 .. height lastGrid - 1]
          moves    = [GridlockMove player colour (x,y) | colour <- colours, x <- xs, y <- ys]

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

setPlayers :: GameRecordExt -> [Player] -> GameRecordExt
setPlayers record newPlayers = record {gameRecord = (gameRecord record) {players = newPlayers}}

getPlayers :: GameRecordExt -> [Player]
getPlayers record = players (gameRecord record)

setGrids :: GameRecordExt -> [Grid] -> GameRecordExt
setGrids record newGrids = record {gameRecord = (gameRecord record) {grids = newGrids}}

getGrids :: GameRecordExt -> [Grid]
getGrids record = grids (gameRecord record)

setColours :: GameRecordExt -> Set Colour -> GameRecordExt
setColours record newColours = record {gameRecord = (gameRecord record) {colours = newColours}}

getColours :: GameRecordExt -> Set Colour
getColours record = colours (gameRecord record)

isCellEqualTo :: Grid -> Coord -> Cell -> Bool
isCellEqualTo grid coord cell = case Map.lookup coord (rep grid) of
                                    Just c -> c == cell
                                    _ -> False

isCoordInBounds :: Grid -> Coord -> Bool
isCoordInBounds grid (x,y) = 0 <= x && x < width grid && 0 <= y && y < height grid

doesPlayerExist :: GameRecordExt -> Player -> Bool
doesPlayerExist record player = case find (==player) (getPlayers record) of
                                (Just _) -> True
                                _ -> False

isGameRecordValid :: GameRecordExt -> Bool
isGameRecordValid (GameRecordExt True _ _ _) = True
isGameRecordValid _ = False

updateGrid :: Grid -> GridlockMove -> Grid
updateGrid grid (GridlockMove _ colour position) = grid {rep = Map.insert position (Filled colour) (rep grid)}

gridlockMoveToString :: GridlockMove -> String
gridlockMoveToString (GridlockMove player colour coord) = "GridlockMove { player = " ++ player ++ ", colour = " ++ show colour ++ ", coord = " ++ show coord ++ " }"