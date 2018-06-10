{-# language FlexibleContexts #-}
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
import           Reflex.Dom                         (MonadWidget, Dynamic, Event)
import qualified Reflex.Dom                         as RD
import qualified GHCJS.DOM                          as JSDOM
import qualified GHCJS.DOM.JSFFI.Generated.Document as Doc
import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified GHCJS.DOM.CanvasRenderingContext2D as C
import           JavaScript.Web.Location            (getWindowLocation, reload)
import           GHCJS.DOM.Types                    (JSM, liftJSM, toElement)
import           Reflex.Dom.CanvasBuilder.Types     (HasRenderFn, RenderContext)
import qualified Reflex.Dom.CanvasBuilder.Types     as Canvas
import qualified Reflex.Dom.CanvasDyn               as CDyn

import           Canvas.Types
import           Cell
import           Cell.Types
import           Maze

drawWithCx
  :: ( MonadWidget t m
     , HasRenderFn c ( RenderContext c )
     )
  => Dynamic t ( RenderContext c )
  -> Dynamic t ( RenderContext c -> Double -> JSM a )
  -> Event t ()
  -> m ( Event t a )
drawWithCx dContext dAction eApply =
  let
    nextFrame cx f = liftJSM $
      JSDOM.nextAnimationFrame (f cx)
  in
    -- The original 'drawWithCx' code:
    {-
    RD.performEvent
    ( nextFrame
      <$> R.current dContext
      <*> R.current dAction
      <@ eApply
    )
    -}
    -- where '<@' is 'tag'.
    --
    -- From the 'tagPromptlyDyn' docstring:
    {-
    `tagPromptlyDyn d e` differs from `tag (current d) e` in the case that e is
    firing at the same time that d is changing. With `tagPromptlyDyn d e`, the
    *new* value of d will replace the value of e, whereas with `tag (current d) e`,
    the *old* value will be used, since the Behavior won't be updated until the end
    of the frame.
    -}
    RD.performEvent $ RD.tagPromptlyDyn
      (nextFrame <$> dContext <*> dAction) eApply

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
  let canvasH  = CanvasHeight 51  -- 11
      canvasW  = CanvasWidth  51  -- 11
      canvasId = CanvasId "maze"

      canvasAttrs = makeCanvasAttrs canvasH canvasW canvasId

  -- Create the canvas element.
  (_, (elCanvas, _)) <- RD.elAttr' "div" Map.empty $
    RD.elAttr' "canvas" canvasAttrs RD.blank

  -- Get the context.
  dyn2D <- fmap (Canvas._canvasInfo_context)
    -- Create a canvas for a 2D drawing.
    <$> CDyn.dContext2d (Canvas.CanvasConfig elCanvas [])

  -- Maze.
  -- Generate a maze.
  cells <- liftIO $ maze canvasH canvasW

  -- Perform the action when the page is loaded.
  evApply <- RD.getPostBuild

  -- Buttons.
  -- Cursor handling.
  evLeftBtn  <- RD.button "left"
  evDownBtn  <- RD.button "down"
  evUpBtn    <- RD.button "up"
  evRightBtn <- RD.button "right"

  -- Help.
  let helpAttrs =
        Map.singleton "style" $ Text.intercalate "; "
          [ "position: relative"
          , "padding-left: 20px"
          , "font-size: small"
          ]
  RD.elAttr "span" helpAttrs $ do
      evHelpBtn <- RD.button "?"
      let msg = "Find your way through the maze!"
             <> " Use hjkl, arrows, or buttons to move."
          help True  = msg
          help False = ""
      RD.el "br" $ RD.blank
      RD.dynText . (fmap help) =<<
        RD.foldDyn (\_ b -> not b) False evHelpBtn

  -- Focus on the canvas without additional clicks.
  -- XXX: Use safe alternatives.  IIUC, these might break if the target is not a
  -- browser.
  doc <- JSDOM.currentDocumentUnchecked
  rawBody <- toElement <$> Doc.getBodyUnchecked doc
  body <- RD.wrapRawElement rawBody RD.def

  let evLeft  = RD.keydown RD.KeyH      body
             <> RD.keydown RD.ArrowLeft body
             <> evLeftBtn
      evDown  = RD.keydown RD.KeyJ      body
             <> RD.keydown RD.ArrowDown body
             <> evDownBtn
      evUp    = RD.keydown RD.KeyK      body
             <> RD.keydown RD.ArrowUp   body
             <> evUpBtn
      evRight = RD.keydown RD.KeyL       body
             <> RD.keydown RD.ArrowRight body
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

  _ <- drawWithCx dyn2D (action cells <$> dynCursor) evCombined

  return ()
