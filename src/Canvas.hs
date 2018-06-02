{-# language OverloadedStrings #-}

module Canvas where

import           Control.Monad                      (forM_, when)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.Map                           as Map
import           Data.Map                           (Map)
import           Data.Semigroup                     ((<>))
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Reflex                             as R
import           Reflex.Dom                         (MonadWidget)
import qualified Reflex.Dom                         as RD
import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as C
import           JavaScript.Web.Location            (getWindowLocation, reload)
import           GHCJS.DOM.Types                    (JSM)
--import           GHCJS.DOM.JSFFI.Generated.HTMLElement (focus)
import qualified Reflex.Dom.CanvasBuilder.Types     as Canvas
import qualified Reflex.Dom.CanvasDyn               as CDyn

import           Canvas.Types
import           Cell
import           Cell.Types
import           Maze

-- | The size of a cell.
step :: Int
step = 10

makeCanvasAttrs :: CanvasHeight -> CanvasWidth -> CanvasId -> Map Text Text
makeCanvasAttrs (CanvasHeight h) (CanvasWidth w) (CanvasId i) =
  Map.fromList
    [ ("height", Text.pack $ show $ h * step)
    , ("width" , Text.pack $ show $ w * step)
    , ("id"    , Text.pack $ show i)
    ]

cell :: CanvasRenderingContext2D -> Coord -> Color -> JSM ()
cell cx (Coord x y) (Color color) = do
  C.setFillStyle cx color
  C.fillRect cx (realToFrac $ x * step) (realToFrac $ y * step)
    (realToFrac step) (realToFrac step)

path :: CanvasRenderingContext2D -> Coord -> JSM ()
path cx coord = cell cx coord (Color "lightblue")

end :: CanvasRenderingContext2D -> Coord -> JSM ()
end cx coord = cell cx coord (Color "darkgray")

cursor :: CanvasRenderingContext2D -> Coord -> JSM ()
cursor cx coord = cell cx coord (Color "salmon")

-- XXX: Do not redraw the whole canvas on each move.

-- | Draw the maze.
action :: Set Cell -> Coord -> CanvasRenderingContext2D -> Double -> JSM ()
action cells coord cx _ = do
  -- Draw the path.
  forM_ cells $ path cx

  -- Draw the end.
  when (not $ Set.null cells) $
    end cx $ Set.findMax cells

  -- Backup the initial state.
  C.save cx

  print $ "drawing cursor: " ++ show coord  -- XXX: debug
  cursor cx coord

  -- Reload the page when the end is reached.
  when (not (Set.null cells) &&
        coord == Set.findMax cells) $
    reload True =<< getWindowLocation

  -- Restore the initial state.
  C.restore cx

-- XXX: Cell coordinates are repeated when direction changes.
-- It looks like the current event forces the previous one.
move :: (Coord -> Coord) -> Set Cell -> CanvasHeight -> CanvasWidth -> Coord -> Coord
move f cells h w cur =
  let new = f cur
  in if isValidCell h w new && Set.member new cells
     then new
     else cur

moveLeft :: Set Cell -> CanvasHeight -> CanvasWidth -> Coord -> Coord
moveLeft = move f
  where
    f (Coord x y) = Coord (x - 1) y

moveRight :: Set Cell -> CanvasHeight -> CanvasWidth -> Coord -> Coord
moveRight = move f
  where
    f (Coord x y) = Coord (x + 1) y

moveUp :: Set Cell -> CanvasHeight -> CanvasWidth -> Coord -> Coord
moveUp = move f
  where
    f (Coord x y) = Coord x (y - 1)

moveDown :: Set Cell -> CanvasHeight -> CanvasWidth -> Coord -> Coord
moveDown = move f
  where
    f (Coord x y) = Coord x (y + 1)

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  -- XXX: Make these arguments and move to 'Main'.
  -- XXX: Because of how 'maze' works currently, these must be odd.
  let canvasH  = CanvasHeight 11  -- 61
      canvasW  = CanvasWidth  11  -- 61
      canvasId = CanvasId "maze"

      canvasAttrs = makeCanvasAttrs canvasH canvasW canvasId

  -- Create the canvas element.
  (elOut, (elIn, _)) <- RD.elAttr' "div" (Map.singleton "tabindex" "0") $
    RD.elAttr' "canvas" canvasAttrs RD.blank

  -- XXX: Focus on the canvas without additional clicks.

  -- Get the context.
  dyn2D <- fmap (Canvas._canvasInfo_context)
    -- Create a canvas for a 2D drawing.
    <$> CDyn.dContext2d (Canvas.CanvasConfig elIn [])

  -- Maze.
  -- Generate a maze.
  cells <- liftIO $ maze canvasH canvasW

  -- Perform the action when the page is loaded.
  evApply <- RD.getPostBuild

  -- Cursor handling.
  evLeftBtn  <- RD.button "left"
  evDownBtn  <- RD.button "down"
  evUpBtn    <- RD.button "up"
  evRightBtn <- RD.button "right"

  let evLeft  = RD.keydown RD.KeyH      elOut
             <> RD.keydown RD.ArrowLeft elOut
             <> evLeftBtn
      evDown  = RD.keydown RD.KeyJ      elOut
             <> RD.keydown RD.ArrowDown elOut
             <> evDownBtn
      evUp    = RD.keydown RD.KeyK      elOut
             <> RD.keydown RD.ArrowUp   elOut
             <> evUpBtn
      evRight = RD.keydown RD.KeyL       elOut
             <> RD.keydown RD.ArrowRight elOut
             <> evRightBtn

  -- Assumes there's always a cell at this location.
  let start = Coord 0 0
  dynCursor <- R.foldDyn ($) start $ R.mergeWith (.)
    [ moveLeft  cells canvasH canvasW <$ evLeft
    , moveDown  cells canvasH canvasW <$ evDown
    , moveUp    cells canvasH canvasW <$ evUp
    , moveRight cells canvasH canvasW <$ evRight
    ]

  let evMoved = R.updated dynCursor
      evCombined = RD.leftmost [evApply, () <$ evMoved]

  _ <- CDyn.drawWithCx dyn2D (action cells <$> dynCursor) evCombined

  return ()
