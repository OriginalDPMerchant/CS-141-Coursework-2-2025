module Gridlock.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List

import Data.Void
import Data.Char
import Data.Maybe
import Gridlock.Types
import Gridlock.GameLogic

type Parser = Parsec Void String

-- Parse the file into a GameRecord, parses the file into a GameRecordExt as an intermediary step. if isValid is false the parsing will fail
parseGame :: Parser GameRecord
parseGame = do
    input <- parseGameExt
    case input of
        (GameRecordExt True _ _ gameRecord) -> return gameRecord
        (GameRecordExt False errorMsg _ _)  -> fail errorMsg

-- Parse the file into a GameRecordExt, parses the file into AST as an intermediary step
parseGameExt :: Parser GameRecordExt
parseGameExt = do
    input <- parseGridlockASTNode
    case input of
        (Gridlock x) -> return $ makeGameRecord (GameRecordExt True "" "" (GameRecord [] [] Set.empty)) (Gridlock x)
        _            -> fail "Must Start with GRIDLOCK"

--                          Semantic Analysis
-- //////////////////////////////////////////////////////////////

-- Converts the AST into a GameRecordExt, recursively traverses the AST until it reaches an "End" node or encounters an illegal move, 
-- in the latter case the isValid flag is set to false
makeGameRecord :: GameRecordExt -> GridlockASTNode -> GameRecordExt

--When We Encounter an error stop recurrsion
makeGameRecord (GameRecordExt False a b c) _ = GameRecordExt False a b c

makeGameRecord record (Gridlock next) = makeGameRecord record next

makeGameRecord record (Players (player1, player2) next) = makeGameRecord newRecord next
    where newRecord = (setPlayers record [player1, player2]) {lastPlayer = player2}

makeGameRecord record (GridSize (m, n) next) = makeGameRecord newRecord next
    where grid      = Map.fromList [((x, y), Empty) | x <- [0 .. m - 1], y <- [0 .. n - 1]]
          newRecord = setGrids record [Grid m n grid]

makeGameRecord record (Colours colour next) = makeGameRecord (setColours record colour) next

makeGameRecord record (Move move next) = makeGameRecord (playMove record move) next

makeGameRecord record (Lose player next) = makeGameRecord (validateLoser record player) next

makeGameRecord record (Win player next) = makeGameRecord (validateWinner record player) next

makeGameRecord record _ = record

-- checks that the correct person lost
validateLoser :: GameRecordExt -> Player -> GameRecordExt
validateLoser record player  | not isLoserCorrect               = record {isValid = False, errorMsg = "Wrong Loser"}
                             | possibleMovesExist record player = record {isValid = False, errorMsg = "Possible Moves Exist"}
                             | otherwise                        = record
    where isLoserCorrect     = lastPlayer record /= player && doesPlayerExist record player

-- checks that the correct person won
validateWinner :: GameRecordExt -> Player -> GameRecordExt
validateWinner record player  | isWinnerCorrect = record
                              | otherwise      = record {isValid = False, errorMsg = "Wrong Winner"}
    where isWinnerCorrect     = lastPlayer record == player && doesPlayerExist record player

--                          Syntactic Analysis / Building AST
-- ////////////////////////////////////////////////////////////////////////////////////

-- Parses a string into the Abstract Syntax Tree
parseGridlockASTNode :: Parser GridlockASTNode
parseGridlockASTNode = try parseGridlock 
               <|> try parsePlayers 
               <|> try parseGrid 
               <|> try parseColours 
               <|> try parseMove 
               <|> try parseLose 
               <|> try parseWin

parseGridlock :: Parser GridlockASTNode
parseGridlock = do
    _ <- many newline
    _ <- string "GRIDLOCK"
    Gridlock <$> parseGridlockASTNode

parsePlayers :: Parser GridlockASTNode
parsePlayers = do
    _  <- many newline
    _  <- string "Players:"
    _  <- space
    p1 <- readUntilChar ','
    _  <- char ','
    _  <- space
    p2 <- readUntilChar '.'
    _  <- char '.'
    if isNameValid p1 && isNameValid p2 then
        Players (p1, p2) <$> parseGridlockASTNode
    else
        fail "Player Names must only contain Alpha Numeric Characters"

parseGrid :: Parser GridlockASTNode
parseGrid = do
    _  <- many newline
    _ <- string "Grid:"
    _ <- space
    m <- some digitChar
    _ <- char 'x'
    n <- some digitChar
    _ <- char '.'
    GridSize (read m, read n) <$> parseGridlockASTNode

parseColours :: Parser GridlockASTNode
parseColours = do
    _  <- many newline
    _  <- string "Colours:"
    _  <- space
    c  <- readColours [] ", " "."
    Colours (Set.fromList c) <$> parseGridlockASTNode

parseMove :: Parser GridlockASTNode
parseMove = do
    _  <- many newline
    p <- readUntilChar ' '
    _ <- space
    _ <- string "plays"
    _ <- space
    c <- readColour
    _ <- space
    _ <- string "at"
    _ <- space
    _ <- char '('
    x <- some digitChar
    _ <- char ','
    y <- some digitChar
    _ <- char ')'
    _ <- char '.'
    Move (GridlockMove p c (read x, read y)) <$> parseGridlockASTNode

parseLose :: Parser GridlockASTNode
parseLose = do
    _  <- many newline
    p <- readUntilChar ' '
    _ <- space
    _ <- string "cannot move."
    Lose p <$> parseGridlockASTNode

parseWin :: Parser GridlockASTNode
parseWin = do
    _  <- many newline
    p <- readUntilChar ' '
    _ <- space
    _ <- string "wins!"
    _  <- many newline
    _ <- eof
    return $ Win p End

--                          Helper Functions
-- //////////////////////////////////////////////////////////////

-- Checks that a player name is valid (no spaces no non alpha-numeric characters)
isNameValid :: Player -> Bool
isNameValid name = not (null name) && all isAlphaNum name

-- consumes characters until we encounter a particular character
readUntilChar :: Char -> Parser String
readUntilChar c = some (satisfy (/= c))

-- parses a string to a colour
readColour :: Parser Colour
readColour = readRed <|> readGreen <|> readYellow <|> readBlue <|> readMagenta<|> readCyan
    where readRed     = string' "red" >> return Red
          readGreen   = string' "green" >> return Green
          readYellow  = string' "yellow" >> return Yellow
          readBlue    = string' "blue" >> return Blue
          readMagenta = string' "magenta" >> return Magenta
          readCyan    = string' "cyan" >> return Cyan

-- parses a list of colours using a certain string as a seperator and another string to indicate the end
readColours :: [Colour] -> String -> String -> Parser [Colour]
readColours colours seperator end = try readHead <|> readTail
    where readHead = do
                    newColour <- readColour
                    _ <- string seperator
                    readColours (colours ++ [newColour]) seperator end 
          readTail = do
                    newColour <- readColour
                    _ <- string end
                    return (colours ++ [newColour])