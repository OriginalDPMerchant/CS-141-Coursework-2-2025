module Gridlock.DrawGrid where

import Gridlock.ColourSquares
import Gridlock.Types

-- Hint: These might come in handy :)
import Data.Map (Map)
import qualified Data.Map as Map

getCellChar :: Maybe Cell -> String
getCellChar (Just (Filled x)) = square x
getCellChar _ = " "

drawGrid :: Grid -> String
drawGrid grid          = unlines (border : map row [0 .. height grid - 1]) ++ border
    where border       = "+" ++ concat ["-" | _ <- [0 .. width grid - 1]] ++ "+"
          row y        = "|" ++ concatMap (`readCell` y) [0 .. width grid - 1] ++ "|"
          readCell x y = getCellChar $ Map.lookup (x, y) (rep grid)

getCellCharNoAnsi :: Maybe Cell -> String
getCellCharNoAnsi (Just (Filled x)) = squareNoAnsi x
getCellCharNoAnsi _ = " "

drawGridNoAnsi :: Grid -> String
drawGridNoAnsi grid          = unlines (border : map row [0 .. height grid - 1]) ++ border
    where border       = "+" ++ concat ["-" | _ <- [0 .. width grid - 1]] ++ "+"
          row y        = "|" ++ concatMap (`readCell` y) [0 .. width grid - 1] ++ "|"
          readCell x y = getCellCharNoAnsi $ Map.lookup (x, y) (rep grid)