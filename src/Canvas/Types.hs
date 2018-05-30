module Canvas.Types where

import Data.Text (Text)
import GHCJS.DOM.Types (JSString)

newtype CanvasHeight = CanvasHeight { fromCanvasHeight :: Int }
  deriving (Eq, Show)

newtype CanvasWidth = CanvasWidth { fromCanvasWidth :: Int }
  deriving (Eq, Show)

newtype CanvasId = CanvasId { fromCanvasId :: Text }
  deriving (Eq, Show)

data Coord = Coord
  { _coord_x :: Int
  , _coord_y :: Int
  } deriving (Eq, Ord, Show)

newtype Color = Color { fromColor :: JSString }
  deriving (Eq, Show)
