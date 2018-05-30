{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main where

import           Control.Lens                       ((^.))
import qualified Data.Map                           as Map
import qualified Data.Text                          as Text
import           Data.Text                          (Text)
import qualified Reflex.Dom                         as RD
import           Reflex.Dom                         (MonadWidget)
import qualified Reflex.Dom.CanvasBuilder.Types     as Canvas
import qualified Reflex.Dom.CanvasDyn               as CDyn
import qualified GHCJS.DOM.CanvasRenderingContext2D as C
import           GHCJS.DOM.Types                    (JSString)


main :: IO ()
main = do RD.mainWidget bodyElement

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  let canvasH  = 600 :: Int
      canvasW  = 600 :: Int
      canvasId = "maze" :: Text

      canvasAttrs = Map.fromList
        [ ("height", Text.pack $ show canvasH)
        , ("width" , Text.pack $ show canvasW)
        , ("id"    , canvasId)
        ]

  -- Create the canvas element.
  elCanvas <- fst <$> RD.elAttr' "canvas" canvasAttrs RD.blank

  -- Get the context.
  dyn2D <- fmap (^. Canvas.canvasInfo_context)
       -- Create a canvas for a 2D drawing.
       <$> CDyn.dContext2d (Canvas.CanvasConfig elCanvas [])

  let step = 10

      cell cx x y color = do
        C.setFillStyle cx color
        C.fillRect cx x y step step

      border cx x y = cell cx x y ("darkgray"  :: JSString)
      path   cx x y = cell cx x y ("lightblue" :: JSString)
      cursor cx x y = cell cx x y ("fuchsia"   :: JSString)

      -- Specify what to draw.
      action cx _ = do
        border cx 0  0
        border cx 10 0
        border cx 20 0

        path cx 0  10
        path cx 10 10
        path cx 20 10

        cursor cx 10 10

        border cx 0  20
        border cx 10 20
        border cx 20 20

  -- Perform the action when the page is loaded.
  evApply <- RD.getPostBuild

  _ <- CDyn.drawWithCx dyn2D (pure action) evApply

  return ()
