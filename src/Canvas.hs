{-# language OverloadedStrings #-}

module Canvas where

import           Control.Lens                       ((^.))
import           Control.Monad                      (forM_, when)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Map                           as Map
import           Data.Map                           (Map)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Reflex.Dom                         (MonadWidget)
import qualified Reflex.Dom                         as RD
import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as C
import           GHCJS.DOM.Types                    (JSM)
import qualified Reflex.Dom.CanvasBuilder.Types     as Canvas
import qualified Reflex.Dom.CanvasDyn               as CDyn

import           Canvas.Types
import           Maze

makeCanvasAttrs :: CanvasHeight -> CanvasWidth -> CanvasId -> Map Text Text
makeCanvasAttrs (CanvasHeight h) (CanvasWidth w) (CanvasId i) =
  Map.fromList
    [ ("height", Text.pack $ show h)
    , ("width" , Text.pack $ show w)
    , ("id"    , Text.pack $ show i)
    ]

-- | The size of a cell.
step :: Int
step = 10

cell :: CanvasRenderingContext2D -> Coord -> Color -> JSM ()
cell cx (Coord x y) (Color color) = do
  C.setFillStyle cx color
  C.fillRect cx (realToFrac x) (realToFrac y) (realToFrac step) (realToFrac step)

path :: CanvasRenderingContext2D -> Coord -> JSM ()
path cx coord = cell cx coord (Color "lightblue")

end :: CanvasRenderingContext2D -> Coord -> JSM ()
end cx coord = cell cx coord (Color "darkgray")

cursor :: CanvasRenderingContext2D -> Coord -> JSM ()
cursor cx coord = cell cx coord (Color "salmon")

-- | Specify what to draw.
action
  :: CanvasHeight -> CanvasWidth
  -> CanvasRenderingContext2D -> Double -> JSM ()
action (CanvasHeight h) (CanvasWidth w) cx _ = do
  -- Generate a maze.
  cells <- liftIO $ maze (CanvasHeight $ h `div` step) (CanvasWidth $ w `div` step)
  let descCells = Set.toDescList cells

  -- Draw the path.
  forM_ descCells $ \(Coord x y) ->
    let coord = Coord (x * step) (y * step)
    in path cx coord

  -- Draw the end.
  when (not $ null descCells) $
    let Coord x y = head descCells
    in end cx $ Coord (x * step) (y * step)

  -- Draw the cursor.
  -- Assumes there's always a cell at this location.
  let start = Coord 0 0
  cursor cx start

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  -- XXX: Make these arguments and move to 'Main'.
  -- XXX: Because of how 'maze' works currently, these must be odd when divided
  -- by 10.
  let canvasH  = CanvasHeight 110 -- 610
      canvasW  = CanvasWidth  110 -- 610
      canvasId = CanvasId "maze"

      canvasAttrs = makeCanvasAttrs canvasH canvasW canvasId

  -- Create the canvas element.
  elCanvas <- fst <$> RD.elAttr' "canvas" canvasAttrs RD.blank

  -- Get the context.
  dyn2D <- fmap (^. Canvas.canvasInfo_context)
       -- Create a canvas for a 2D drawing.
       <$> CDyn.dContext2d (Canvas.CanvasConfig elCanvas [])

  -- Perform the action when the page is loaded.
  evApply <- RD.getPostBuild

  _ <- CDyn.drawWithCx dyn2D (pure $ action canvasH canvasW) evApply

  return ()
