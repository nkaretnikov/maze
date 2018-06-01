module Cell where

import Canvas.Types
import Cell.Types

isValidCell :: CanvasHeight -> CanvasWidth -> Cell -> Bool
isValidCell (CanvasHeight h) (CanvasWidth w) (Coord x y)
   = x >= 0
  && y >= 0
  && x < w
  && y < h
